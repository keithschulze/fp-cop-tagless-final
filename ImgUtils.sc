// interp.resolvers() ++= Seq(coursier.ivy.IvyRepository.fromPattern(
//   "http://maven.imagej.net/content/groups/public/" +:
//   coursier.ivy.Pattern.default
// ))

import java.util.Base64

import $ivy.`net.imagej:imagej:2.0.0-rc-68`
import $ivy.`net.imglib2:imglib2:5.5.0`
import $ivy.`org.typelevel::spire:0.14.1`, spire.algebra._, spire.implicits._
// import $ivy.`de.christophkraemer:rhino-script-engine:1.1.1`
import algebra._

//import almond.interpreter.api.DisplayData
import almond.api.helpers.Display
import net.imagej.{
  Dataset,
  ImageJ,
  ImgPlus
}
import net.imglib2.algorithm.stats.ComputeMinMax
import net.imglib2.converter.{
  Converters,
  RealUnsignedByteConverter
}
import net.imglib2.img.{
  Img,
  ImgView
}
import net.imglib2.img.array.ArrayImgFactory
import net.imglib2.`type`.NativeType
import net.imglib2.`type`.numeric.RealType
import net.imglib2.`type`.logic.{
    BitType,
    BoolType
}
import net.imglib2.`type`.numeric.integer.{
    ByteType,
    IntType,
    LongType,
    ShortType,
    UnsignedByteType,
    UnsignedIntType,
    UnsignedLongType,
    UnsignedShortType
}
import net.imglib2.`type`.numeric.real.{
    DoubleType,
    FloatType
}
import net.imglib2.view.IntervalView

implicit val ij = new ImageJ()

trait Preview[A] {
  def preview(a: A): Unit
}

object Preview {
  def apply[A](implicit F: Preview[A]): Preview[A] = F

  private[this] def computeMinMax[A <: RealType[A]](img: Img[A]): (Double, Double) = {
    val min: A = img.firstElement.copy()
    val max: A = min.copy()
    ComputeMinMax.computeMinMax(img, min, max)
    (min.getRealDouble, max.getRealDouble)
  }

  implicit def datasetPreview[A <: RealType[A]](implicit ij: ImageJ): Preview[Dataset] =
    new Preview[Dataset] {
      def preview(d: Dataset): Unit = {
        val tFile = java.io.File.createTempFile("jupy",".jpg")
        ij.scifio().datasetIO().save(d, tFile.getPath())
        val imgFIS = new java.io.FileInputStream(tFile.getPath())
        val out = new Array[Byte](imgFIS.getChannel().size().toInt)
        imgFIS.read(out)
        val b64 = Base64.getEncoder().encode(out)
        Display.html("<img src=\"data:image/jpg;base64," + new String(b64) + "\"></img>")
      }
    }


  implicit def imgPlusPreview[A <: RealType[A]](implicit ij: ImageJ): Preview[ImgPlus[A]] =
    new Preview[ImgPlus[A]] {
      def preview(img: ImgPlus[A]): Unit = {
        val d = ij.dataset.create(img)
        Preview[Dataset].preview(d)
      }
    }

  implicit def imgPreview[A <: RealType[A]](implicit ij: ImageJ): Preview[Img[A]] =
    new Preview[Img[A]] {
      def preview(img: Img[A]): Unit = {

        val d = ij.dataset().create(img)
        Preview[Dataset].preview(d)
      }
    }

  implicit def maskPreview(implicit ij: ImageJ): Preview[Img[BitType]] =
    new Preview[Img[BitType]] {
      def preview(img: Img[BitType]): Unit = {
        val (min, max) = computeMinMax(img)
        val conv = new RealUnsignedByteConverter[BitType](min, max)
        val convImg: ImgPlus[UnsignedByteType] =
          ImgPlus.wrap(
            ImgView.wrap(
              Converters.convert(img, conv, new UnsignedByteType),
              new ArrayImgFactory
            )
          )
        val d = ij.dataset().create(convImg)
        Preview[Dataset].preview(d)
      }
    }

  implicit def intervalViewPreview[A <: RealType[A] with NativeType[A]](
    implicit
    ij: ImageJ
  ): Preview[IntervalView[A]] =
    new Preview[IntervalView[A]] {
      def preview(iv: IntervalView[A]): Unit = {
        val min: Array[Long] = Array.ofDim[Long](iv.numDimensions)
        val max: Array[Long] = Array.ofDim[Long](iv.numDimensions)
        iv.min(min)
        iv.max(max)

        val out: Img[A] = new ArrayImgFactory[A]().create(iv, iv.firstElement)

        val outC = out.localizingCursor()
        val ra = iv.randomAccess()

        var adjusted: Array[Long] = Array.ofDim[Long](iv.numDimensions)
        while (outC.hasNext()) {
            outC.fwd();
            outC.localize(adjusted)
            adjusted = adjusted.zip(min).map { case (c, m) => c + m }

            ra.setPosition(adjusted)
            outC.get().set(ra.get())
        }

        Preview[Img[A]].preview(out)
      }
    }

}


final class PreviewOps[A](val a: A) {
  def preview(implicit F: Preview[A]): Unit = F.preview(a)
}

object previews {
  implicit final def previewSyntax[A](a: A): PreviewOps[A] =
    new PreviewOps[A](a)
}

trait BitTypeInstances {
  implicit val bitTypeAlgebra = new BitTypeAlgebra
  implicit val catsKernelStdGroupForBitTyp = new BitTypeGroup
}

class BitTypeAlgebra extends Field.WithDefaultGCD[BitType] {
  def zero: BitType = new BitType(false)
  def one: BitType = new BitType(true)
  def negate(a: BitType): BitType =
      new BitType(!a.get)
  def plus(a: BitType, b: BitType): BitType = {
      val c = a.copy
      c.add(b)
      c
  }
  def times(a: BitType, b: BitType): BitType = {
      val c = a.copy
      c.mul(b)
      c
  }
  def div(a: BitType, b: BitType): BitType = {
      val c = a.copy
      c.div(b)
      c
  }
  override def fromBigInt(n: BigInt): BitType = {
      val out = new BitType()
      out.setBigInteger(n.bigInteger)
      out
  }
  override def fromInt(n: Int): BitType = {
      val out = new BitType()
      out.setInteger(n)
      out
  }
  override def fromDouble(n: Double): BitType = {
      val out = new BitType()
      out.setReal(n)
      out
  }
}

class BitTypeGroup extends CommutativeGroup[BitType] {
  def combine(a: BitType, b: BitType): BitType = {
    val c = a.copy
    c.add(b)
    c
  }
  def empty: BitType = new BitType(false)
  def inverse(a: BitType): BitType = new BitType(!a.get)
}

trait UnsignedByteTypeInstances {
  implicit val unsignedByteTypeAlgebra = new UnsignedByteTypeAlgebra
  implicit val catsKernelStdGroupForUnsignedByteType = new UnsignedByteTypeGroup
}

class UnsignedByteTypeAlgebra extends Field.WithDefaultGCD[UnsignedByteType] {
  def zero: UnsignedByteType = new UnsignedByteType(0)
  def one: UnsignedByteType = new UnsignedByteType(1)
  def negate(a: UnsignedByteType): UnsignedByteType = {
      new UnsignedByteType(-a.get)
  }
  def plus(a: UnsignedByteType, b: UnsignedByteType): UnsignedByteType = {
      val c = a.copy
      c.add(b)
      c
  }
  def times(a: UnsignedByteType, b: UnsignedByteType): UnsignedByteType = {
      val c = a.copy
      c.mul(b)
      c
  }
  def div(a: UnsignedByteType, b: UnsignedByteType): UnsignedByteType = {
      val c = a.copy
      c.div(b)
      c
  }
  override def fromBigInt(n: BigInt): UnsignedByteType = {
      val out = new UnsignedByteType()
      out.setBigInteger(n.bigInteger)
      out
  }
  override def fromInt(n: Int): UnsignedByteType = {
      val out = new UnsignedByteType()
      out.setInteger(n)
      out
  }
  override def fromDouble(n: Double): UnsignedByteType = {
      val out = new UnsignedByteType()
      out.setReal(n)
      out
  }
}

class UnsignedByteTypeGroup extends CommutativeGroup[UnsignedByteType] {
  def combine(a: UnsignedByteType, b: UnsignedByteType): UnsignedByteType = {
    val c = a.copy
    c.add(b)
    c
  }
  def empty: UnsignedByteType = new UnsignedByteType(0)
  def inverse(a: UnsignedByteType): UnsignedByteType = new UnsignedByteType(-a.get)
  override def remove(a: UnsignedByteType, b: UnsignedByteType): UnsignedByteType = {
    val c = a.copy
    c.sub(b)
    c
  }
}

object imglib2 extends BitTypeInstances with UnsignedByteTypeInstances {

  // implicit val boolField: Field[BoolType] = new Field[BoolType] {
  //   def zero: BoolType = new BoolType(false)
  //   def one: BoolType = new BoolType(true)
  //   def negate(a: BoolType): BoolType =
  //       new BoolType(!a.get)
  //   def plus(a: BoolType, b: BoolType): BoolType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: BoolType, b: BoolType): BoolType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: BoolType, b: BoolType): BoolType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromBigInt(n: BigInt): BoolType = {
  //       val out = new BoolType()
  //       out.setBigInteger(n.bigInteger)
  //       out
  //   }
  //   override def fromInt(n: Int): BoolType = {
  //       val out = new BoolType()
  //       out.setInteger(n)
  //       out
  //   }
  //   override def fromDouble(n: Double): BoolType = {
  //       val out = new BoolType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val byteField: Field[ByteType] = new Field[ByteType] {
  //   def zero: ByteType = new ByteType(0.toByte)
  //   def one: ByteType = new ByteType(1.toByte)
  //   def negate(a: ByteType): ByteType = {
  //       val b = a.get.toInt
  //       new ByteType((-b).toByte)
  //   }
  //   def plus(a: ByteType, b: ByteType): ByteType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: ByteType, b: ByteType): ByteType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: ByteType, b: ByteType): ByteType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromBigInt(n: BigInt): ByteType = {
  //       val out = new ByteType()
  //       out.setBigInteger(n.bigInteger)
  //       out
  //   }
  //   override def fromInt(n: Int): ByteType = {
  //       val out = new ByteType()
  //       out.setInteger(n)
  //       out
  //   }
  //   override def fromDouble(n: Double): ByteType = {
  //       val out = new ByteType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val shortField: Field[ShortType] = new Field[ShortType] {
  //   def zero: ShortType = new ShortType(0: Short)
  //   def one: ShortType = new ShortType(1: Short)
  //   def negate(a: ShortType): ShortType = {
  //       new ShortType((-a.get).toShort)
  //   }
  //   def plus(a: ShortType, b: ShortType): ShortType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: ShortType, b: ShortType): ShortType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: ShortType, b: ShortType): ShortType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromBigInt(n: BigInt): ShortType = {
  //       val out = new ShortType()
  //       out.setBigInteger(n.bigInteger)
  //       out
  //   }
  //   override def fromInt(n: Int): ShortType = {
  //       val out = new ShortType()
  //       out.setInteger(n)
  //       out
  //   }
  //   override def fromDouble(n: Double): ShortType = {
  //       val out = new ShortType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val unsignedShortField: Field[UnsignedShortType] = new Field[UnsignedShortType] {
  //   def zero: UnsignedShortType = new UnsignedShortType(0)
  //   def one: UnsignedShortType = new UnsignedShortType(1)
  //   def negate(a: UnsignedShortType): UnsignedShortType = {
  //       new UnsignedShortType(-a.get)
  //   }
  //   def plus(a: UnsignedShortType, b: UnsignedShortType): UnsignedShortType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: UnsignedShortType, b: UnsignedShortType): UnsignedShortType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: UnsignedShortType, b: UnsignedShortType): UnsignedShortType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromBigInt(n: BigInt): UnsignedShortType = {
  //       val out = new UnsignedShortType()
  //       out.setBigInteger(n.bigInteger)
  //       out
  //   }
  //   override def fromInt(n: Int): UnsignedShortType = {
  //       val out = new UnsignedShortType()
  //       out.setInteger(n)
  //       out
  //   }
  //   override def fromDouble(n: Double): UnsignedShortType = {
  //       val out = new UnsignedShortType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val intField: Field[IntType] = new Field[IntType] {
  //   def zero: IntType = new IntType(0)
  //   def one: IntType = new IntType(1)
  //   def negate(a: IntType): IntType = {
  //       new IntType(-a.get)
  //   }
  //   def plus(a: IntType, b: IntType): IntType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: IntType, b: IntType): IntType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: IntType, b: IntType): IntType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromBigInt(n: BigInt): IntType = {
  //       val out = new IntType()
  //       out.setBigInteger(n.bigInteger)
  //       out
  //   }
  //   override def fromInt(n: Int): IntType = {
  //       val out = new IntType()
  //       out.setInteger(n)
  //       out
  //   }
  //   override def fromDouble(n: Double): IntType = {
  //       val out = new IntType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val uintField: Field[UnsignedIntType] = new Field[UnsignedIntType] {
  //   def zero: UnsignedIntType = new UnsignedIntType(0L)
  //   def one: UnsignedIntType = new UnsignedIntType(1L)
  //   def negate(a: UnsignedIntType): UnsignedIntType = {
  //       new UnsignedIntType(-a.get)
  //   }
  //   def plus(a: UnsignedIntType, b: UnsignedIntType): UnsignedIntType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: UnsignedIntType, b: UnsignedIntType): UnsignedIntType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: UnsignedIntType, b: UnsignedIntType): UnsignedIntType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromBigInt(n: BigInt): UnsignedIntType = {
  //       val out = new UnsignedIntType()
  //       out.setBigInteger(n.bigInteger)
  //       out
  //   }
  //   override def fromInt(n: Int): UnsignedIntType = {
  //       val out = new UnsignedIntType()
  //       out.setInteger(n)
  //       out
  //   }
  //   override def fromDouble(n: Double): UnsignedIntType = {
  //       val out = new UnsignedIntType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val longField: Field[LongType] = new Field[LongType] {
  //   def zero: LongType = new LongType(0L)
  //   def one: LongType = new LongType(1L)
  //   def negate(a: LongType): LongType = {
  //       new LongType(-a.get)
  //   }
  //   def plus(a: LongType, b: LongType): LongType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: LongType, b: LongType): LongType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: LongType, b: LongType): LongType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromBigInt(n: BigInt): LongType = {
  //       val out = new LongType()
  //       out.setBigInteger(n.bigInteger)
  //       out
  //   }
  //   override def fromInt(n: Int): LongType = {
  //       val out = new LongType()
  //       out.setInteger(n)
  //       out
  //   }
  //   override def fromDouble(n: Double): LongType = {
  //       val out = new LongType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val floatField: Field[FloatType] = new Field[FloatType] {
  //   def zero: FloatType = new FloatType(0L)
  //   def one: FloatType = new FloatType(1L)
  //   def negate(a: FloatType): FloatType = {
  //       new FloatType(-a.get)
  //   }
  //   def plus(a: FloatType, b: FloatType): FloatType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: FloatType, b: FloatType): FloatType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: FloatType, b: FloatType): FloatType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromDouble(n: Double): FloatType = {
  //       val out = new FloatType()
  //       out.setReal(n)
  //       out
  //   }
  // }

  // implicit val doubleField: Field[DoubleType] = new Field[DoubleType] {
  //   def zero: DoubleType = new DoubleType(0L)
  //   def one: DoubleType = new DoubleType(1L)
  //   def negate(a: DoubleType): DoubleType = {
  //       new DoubleType(-a.get)
  //   }
  //   def plus(a: DoubleType, b: DoubleType): DoubleType = {
  //       val c = a.copy
  //       c.add(b)
  //       c
  //   }
  //   def times(a: DoubleType, b: DoubleType): DoubleType = {
  //       val c = a.copy
  //       c.mul(b)
  //       c
  //   }
  //   def div(a: DoubleType, b: DoubleType): DoubleType = {
  //       val c = a.copy
  //       c.div(b)
  //       c
  //   }
  //   override def fromDouble(n: Double): DoubleType = {
  //       val out = new DoubleType()
  //       out.setReal(n)
  //       out
  //   }
  // }
}
