package com.scte35

/**
 * Exception to be thrown in case Decoding goes wrong.
 */
class DecodingException(message: String, cause: Throwable) extends RuntimeException(message, cause) {
  def this(message: String) = this(message, null)
}
