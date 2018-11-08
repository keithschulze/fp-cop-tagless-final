interp.repositories() ++= Seq(coursier.maven.MavenRepository("http://maven.imagej.net/content/groups/public/"))

@

import $plugin.$ivy.`org.spire-math::kind-projector:0.9.8`
import $ivy.`org.typelevel::cats-core:1.4.0`, cats._, cats.implicits._
import $ivy.`org.typelevel::cats-effect:1.0.0`, cats.effect.{IO}
import $ivy.`net.imglib2:imglib2:5.5.0`
import $ivy.`io.scif:scifio:0.37.2`
import $ivy.`net.imagej:imagej:2.0.0-rc-68`

import java.nio.file.Path
import scala.collection.JavaConverters._
import scala.util.{Try, Success, Failure}

import io.scif.img.ImgOpener

import net.imagej.ImageJ
import net.imglib2.algorithm.labeling.{ConnectedComponents}
import net.imglib2.img.{Img, ImgView}
import net.imglib2.img.array.ArrayImgFactory
import net.imglib2.roi.labeling.{ImgLabeling, LabelRegions}
import net.imglib2.`type`.NativeType
import net.imglib2.`type`.numeric.{RealType, IntegerType}
import net.imglib2.`type`.numeric.integer.{IntType, UnsignedByteType}
import net.imglib2.`type`.logic.BitType
import net.imglib2.view.Views

val ij = new ImageJ()

// Algebra
trait ImgProcessor[F[_], A] {
    def readImg(path: String): F[Img[A]]
    def extractChannel(img: Img[A], channel: Int): F[Img[A]]
    def smooth(img: Img[A], sigma: Double): F[Img[A]]
    def threshold(img: Img[A], value: A): F[Img[BitType]]
    def label(mask: Img[BitType]): F[ImgLabeling[Int, BitType]]
    def filter(label: ImgLabeling[Int, BitType], minSize: Double): F[ImgLabeling[Int, BitType]]
    def extractSizes(label: ImgLabeling[Int, BitType]): F[List[Double]]
}

object ImgProcessor {
    def apply[F[_], A](implicit ev: ImgProcessor[F, A]): ImgProcessor[F, A] = ev
}

implicit val ubMonoid = new Monoid[UnsignedByteType] {
    def empty: UnsignedByteType = new UnsignedByteType(0)
    def combine(a: UnsignedByteType, b: UnsignedByteType): UnsignedByteType = {
        val c = a.copy()
        c.add(b)
        c
    }
}

class IPInterpreter[F[_], A <: RealType[A] with NativeType[A]](
    implicit
    F: MonadError[F, Throwable],
    M: Monoid[A]
  ) extends ImgProcessor[F, A] {
    def readImg(path: String): F[Img[A]] = F.catchNonFatal {
        val io = new ImgOpener()
        io.openImg(path.toString, M.empty)
    }

    def extractChannel(img: Img[A], channel: Int): F[Img[A]] = F.pure {
        ImgView.wrap(Views.hyperSlice(img, 2, channel), img.factory())
    }

    def smooth(img: Img[A], sigma: Double): F[Img[A]] = F.pure {
        ImgView.wrap(ij.op().filter().gauss(img, sigma), img.factory())
    }

    def threshold(img: Img[A], value: A): F[Img[BitType]] = F.pure {
        val ii = ij.op().threshold().apply(img, value)
        val out = ij.op().create().img(ii)
        ij.op().copy().iterableInterval(out, ii)
        out
    }

    def label(mask: Img[BitType]): F[ImgLabeling[Int, BitType]] = F.pure {
        ij.op().labeling().cca(mask, ConnectedComponents.StructuringElement.EIGHT_CONNECTED)
    }

    def filter(label: ImgLabeling[Int, BitType], minSize: Double): F[ImgLabeling[Int, BitType]] = F.pure {
        val regions = new LabelRegions(label).iterator().asScala.toList
        val filteredImg = new ArrayImgFactory[BitType]().create(label, new BitType)
        regions.filter( r => ij.op().geom().size(r).get() >= minSize)
            .foreach(r => {
                val rCursor = r.localizingCursor
                val rRA = r.randomAccess
                val fiRA = filteredImg.randomAccess

                while(rCursor.hasNext()) {
                    rCursor.fwd()
                    rRA.setPosition(rCursor)
                    fiRA.setPosition(rCursor)
                    fiRA.get().set(rRA.get().get())
                }
            })
        ij.op().labeling().cca(filteredImg, ConnectedComponents.StructuringElement.EIGHT_CONNECTED)
    }

    def extractSizes(label: ImgLabeling[Int, BitType]): F[List[Double]] = F.pure {
        val regions = new LabelRegions(label).iterator().asScala.toList
        regions.map( r => ij.op().geom().size(r).get())
    }
}

def program[F[_]: Monad](ip: ImgProcessor[F, UnsignedByteType]): F[Tuple2[Int, List[Double]]] = for {
    img    <- ip.readImg("http://imagej.net/images/FluorescentCells.jpg")
    n      <- ip.extractChannel(img, 2)
    s      <- ip.smooth(n, 2.0)
    m      <- ip.threshold(s, new UnsignedByteType(60))
    l      <- ip.label(m)
    filtL  <- ip.filter(l, 100.0)
    result <- ip.extractSizes(filtL)
} yield (result.size, result)

val resultIO = program[IO](new IPInterpreter[IO, UnsignedByteType])

println(s"Result from IO: ${resultIO.unsafeRunSync}")

val resultTry = program[Try](new IPInterpreter)

resultTry match {
  case Success(t) => println(s"Result from Try: $t")
  case Failure(e) => println(e)
}

