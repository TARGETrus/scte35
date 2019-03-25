package com.scte35

import org.apache.commons.codec.DecoderException

import scala.reflect.ClassTag

/**
 * Wrapper object for the String of bits.
 * No complex checking logic, so it should be used with caution.
 *
 * @param bitString bits String, i.e. "1101001111110001".
 */
class BitString(bitString: String) {

  /** Pointer position. Can be modified outside the class, so keep your eye on it. */
  var pos: Int = 0
  /** Bit String length. */
  val len: Int = bitString.length

  // Usage of ClassTags is not the best approach, but's enough to get things done.
  private val IntClass = ClassTag.Int
  private val LongClass = ClassTag.Long
  private val BooleanClass = ClassTag.Boolean
  private val ComplexClass = ClassTag.AnyRef

  /**
   * Reads value from bitString of specified size and shifts pointer.
   *
   * @param size how many bits should be read.
   * @param tag ClassTag of the type of the value we want to read.
   * @tparam T the type of the value we want to read.
   * @return value of type T.
   */
  def read[T: ClassTag](size: Int)(implicit tag: ClassTag[T]): T = {

    if (size < 0)
      throw new DecoderException(s"Can't read negative amount: $size.")
    if (size > len - pos)
      throw new DecoderException(s"Can't read $size bits, only ${len-pos} left.")

    // read "size" amount of bits from current pointer position, move pointer forward.
    val bits = bitString.slice(pos, pos + size)
    pos += size

    tag match {
      // Use it if size in bytes is less than "32".
      case IntClass =>
        Integer.parseInt(bits, 2).asInstanceOf[T]
      // Use it if size in bytes is more than "32".
      case LongClass =>
        java.lang.Long.parseLong(bits, 2).asInstanceOf[T]
      // 1-Bit boolean values.
      case BooleanClass =>
        bits match {
          case "1" => true.asInstanceOf[T]
          case "0" => false.asInstanceOf[T]
        }
      // Any AnyRef type will be treated as HEX.
      case ComplexClass =>
        BigInt(bits, 2).toString(16).asInstanceOf[T]
      case _ => throw new DecoderException(s"Unknown type: $tag")
    }
  }
}
