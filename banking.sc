interp.repositories() ++= Seq(coursier.maven.MavenRepository("http://maven.imagej.net/content/groups/public/"))

@

import $plugin.$ivy.`org.spire-math::kind-projector:0.9.8`
import $ivy.`org.typelevel::cats-core:1.4.0`, cats._, cats.implicits._, cats.data.{EitherK, NonEmptyVector}
import $ivy.`org.typelevel::cats-free:1.4.0`
import $ivy.`org.typelevel::cats-effect:1.0.0`, cats.effect.{IO}

import scala.collection.mutable.Map
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}


sealed trait BankingError extends Exception
case class AccountError(message: String) extends BankingError
case class WithdrawError(message: String) extends BankingError
case class DepositError(message: String) extends BankingError
case class TransferError(message: String) extends BankingError


case class Amount(value: BigDecimal)
object Amount {
    def apply(amount: Double): Amount = Amount(BigDecimal(amount))
}


type AccountId = Int
case class Account(id: AccountId, balance: Amount)


case class From[A](value: A)
case class To[A](value: A)
type TransferResult = Either[BankingError, (From[Amount], To[Amount])]


trait Banking[F[_]] {
    def account(accId: AccountId): F[Account]
    def balance(acc: Account): F[Amount]
    def deposit(acc: Account, amount: Amount): F[Amount]
    def withdraw(acc: Account, amount: Amount): F[Amount]
    def transfer(from: From[Account], to: To[Account], amount: Amount): F[TransferResult]
}


class ATMService[F[_]: Monad](bs: Banking[F]) {
    def transferMoolla(fromAccId: AccountId, toAccId: AccountId, amount: Amount): F[TransferResult] =
        for {
            fromAcc <- bs.account(fromAccId)
            toAcc   <- bs.account(toAccId)
            result  <- bs.transfer(From(fromAcc), To(toAcc), amount)
        } yield result
}


object BankingInterp extends Banking[Future] {
    val accs: Map[AccountId, Account] =
        Map(
            0 -> Account(1, Amount(0.0)),
            1 -> Account(2, Amount(100000.0)),
            2 -> Account(3, Amount(1.0))
        )

    def account(accId: AccountId): Future[Account] =
        accs.get(accId) match {
            case Some(acc) => Future.successful(acc)
            case None => Future.failed(AccountError("Account does not exist"))
        }

    def balance(acc: Account): Future[Amount] = Future.successful(acc.balance)
    def deposit(acc: Account, amount: Amount): Future[Amount] =
        if (amount.value >= 0) {
            val newBalance = Amount(acc.balance.value + amount.value)
            accs.update(acc.id, acc.copy(balance=newBalance))
            Future.successful(newBalance)
        } else {
            Future.failed(DepositError("Cannot deposit negative amount."))
        }

    def withdraw(acc: Account, amount: Amount): Future[Amount] =
        if (amount.value >= 0) {
            val newBalance = Amount(acc.balance.value - amount.value)
            accs.update(acc.id, acc.copy(balance=newBalance))
            Future.successful(newBalance)
        } else {
            Future.failed(WithdrawError("Cannot withdraw a negative amount."))
        }

    def transfer(from: From[Account], to: To[Account], amount: Amount): Future[TransferResult] = for {
        out1 <- withdraw(from.value, amount)
        out2 <- deposit(to.value, amount)
    } yield Right((From(out1), To(out2)))
}

val atm = new ATMService(BankingInterp)

val result = atm.transferMoolla(0, 1, Amount(50000))

result onComplete {
  case Success(Right((From(from), To(to)))) =>
    println(
      s"Transfer successful!\nBalance of From Account: $from\nBalance of To Account: $to"
    )
  case Success(Left(m)) => println(s"Transfer failed: $m")
  case Failure(err) => println(err.getMessage)
}

