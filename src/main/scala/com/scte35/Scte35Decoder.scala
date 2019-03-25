package com.scte35

import org.apache.commons.codec.binary.Base64
import scala.collection.mutable

/**
 * SCTE 35 Standard messages encoder.
 * Uses BitString wrapper object to handle bitString in a neat way.
 */
object Scte35Decoder {

  /**
   * Entry point for SCTE 35 Standard message decoder.
   *
   * @param base64in base64 input string.
   * @return decoded content of the SCTE 35 message in mutable Map object.
   */
  def base64Decode(base64in: String): mutable.Map[String, Any] = {
    val b64 = Base64.decodeBase64(base64in)
    var hexString: String = ""
    for (i <- b64.indices) {
      hexString = hexString + "%02X".format(b64(i))
    }
    val bitString = new BitString(BigInt(hexString, 16).toString(2))
    decode35(bitString)
  }

  /**
   * Main method responsible for Decoding.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded data.
   */
  private def decode35(bitString: BitString): mutable.Map[String, Any] = {

    val spliceInfoSection = mutable.Map[String, Any]()

    val tableId = bitString.read[Int](8)

    if (tableId != 0x0FC) {
      throw new DecodingException(s"Invalid Table ID: $tableId, should be 0xFC")
    }

    spliceInfoSection("section_syntax_indicator") = bitString.read[Boolean](1)
    spliceInfoSection("private") = bitString.read[Boolean](1)

    // next 2 bits reserved
    bitString.pos += 2

    spliceInfoSection("section_length") =  bitString.read[Int](12)
    spliceInfoSection("protocol_version") =  bitString.read[Int](8)
    spliceInfoSection("encrypted_packet") = bitString.read[Boolean](1)
    spliceInfoSection("encryption_algorithm") = bitString.read[Int](6)
    spliceInfoSection("pts_adjustment") = bitString.read[Long](33)
    spliceInfoSection("cw_index") = bitString.read[Int](8)
    spliceInfoSection("tier") = bitString.read[AnyRef](12)
    spliceInfoSection("splice_command_length") = bitString.read[Int](12)
    spliceInfoSection("splice_command_type") = bitString.read[Int](8)

    spliceInfoSection("splice_command") = spliceInfoSection("splice_command_type") match {
      case 0x05 =>
        parseSpliceInsert(bitString)
      case 0x06 =>
        parseTimeSignal(bitString)
      case _ =>
        val spliceCommandType = spliceInfoSection("splice_command_type")
        throw new DecodingException(s"splice_command_type: $spliceCommandType not supported.")
    }

    val loopLength = bitString.read[Int](16)

    spliceInfoSection("splice_descriptor_loop_length") = loopLength
    spliceInfoSection("splice_descriptors") = Seq()

    if (loopLength > 0) {
      spliceInfoSection("splice_descriptors") = parseSpliceDescriptors(bitString, loopLength)
    }
    spliceInfoSection
  }

  /**
   * This method is responsible for parsing of "spliceInsert" part of the message.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded "spliceInsert" data.
   */
  private def parseSpliceInsert(bitString: BitString): mutable.Map[String, Any] = {
    val spliceInsert = mutable.Map[String, Any]()
    var components = Seq[mutable.Map[String, Any]]()
    spliceInsert("components") = Seq()

    spliceInsert("splice_event_id") = bitString.read[Long](32)
    spliceInsert("splice_event_cancel_indicator") = bitString.read[Boolean](1)

    // next 7 bits reserved
    bitString.pos += 7

    if (spliceInsert("splice_event_cancel_indicator") == false) {
      spliceInsert("out_of_network_indicator") = bitString.read[Boolean](1)
      spliceInsert("program_splice_flag") = bitString.read[Boolean](1)
      spliceInsert("duration_flag") = bitString.read[Boolean](1)
      spliceInsert("splice_immediate_flag") = bitString.read[Boolean](1)

      // next 4 bits reserved
      bitString.pos += 4

      if (spliceInsert("program_splice_flag") == true &&
        spliceInsert("splice_immediate_flag") == false) {
        spliceInsert("splice_time") = parseSpliceTime(bitString)
      }

      if (spliceInsert("program_splice_flag") == false) {
        val componentCount = bitString.read[Int](8)
        spliceInsert("component_count") = componentCount
        for (i <- 0 until componentCount) {
          val component = mutable.Map[String, Any]()
          component("tag") = bitString.read[Int](8)
          if (spliceInsert("splice_immediate_flag") == true) {
            component("splice_time") = parseSpliceTime(bitString)
          }
          components = components :+ component
        }
        spliceInsert("components") = components
      }

      if (spliceInsert("duration_flag") == true) {
        spliceInsert("break_duration") = parseBreakDuration(bitString)
      }

      spliceInsert("unique_program_id") = bitString.read[Int](16)
      spliceInsert("avail_num") = bitString.read[Int](8)
      spliceInsert("avails_expected") = bitString.read[Int](8)
    }
    spliceInsert
  }

  /**
   * This method is responsible for parsing of "spliceTime" part of the message.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded "spliceTime" data.
   */
  private def parseSpliceTime(bitString: BitString): mutable.Map[String, Any] = {
    val spliceTime = mutable.Map[String, Any]()
    spliceTime("time_specified_flag") = bitString.read[Boolean](1)

    if (spliceTime("time_specified_flag") == true) {
      // next 6 bits reserved
      bitString.pos += 6
      spliceTime("pts_time") = bitString.read[Long](33)
    } else {
      // next 7 bits reserved
      bitString.pos += 7
    }
    spliceTime
  }

  /**
   * This method is responsible for parsing of "breakDuration" part of the message.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded "breakDuration" data.
   */
  private def parseBreakDuration(bitString: BitString): mutable.Map[String, Any] = {
    val breakDuration = mutable.Map[String, Any]()
    breakDuration("auto_return") = bitString.read[Boolean](1)
    // next 6 bits reserved
    bitString.pos += 6
    breakDuration("duration") = bitString.read[Long](33)
    breakDuration
  }

  /**
   * This method is responsible for parsing of "timeSignal" part of the message.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded "timeSignal" data.
   */
  private def parseTimeSignal(bitString: BitString): mutable.Map[String, Any] = {
    val timeSignal = mutable.Map[String, Any]()
    timeSignal("splice_time") = parseSpliceTime(bitString)
    timeSignal
  }

  /**
   * This method is responsible for parsing of "spliceDescriptors" part of the message.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded "spliceDescriptors" data.
   */
  private def parseSpliceDescriptors(bitString: BitString, loopLength: Int): Seq[mutable.Map[String, Any]] = {

    var spliceDescriptors = Seq[mutable.Map[String, Any]]()
    var length = loopLength

    while (length > 2 && bitString.pos < bitString.len - 16) {

      var spliceDescriptor = mutable.Map[String, Any]()

      val descTag = bitString.read[Int](8)
      val descLen = bitString.read[Int](8)
      length -= 2
      length -= descLen

      if (descTag == 0x0)
        spliceDescriptor = parseAvailDescriptor(bitString)
      else if (descTag == 0x2)
        spliceDescriptor = parseSegmentationDescriptor(bitString)
      else
        if (descLen > 0)
          spliceDescriptor("raw") = bitString.read[AnyRef](descLen * 8)

      spliceDescriptor("splice_descriptor_tag") = descTag
      spliceDescriptor("descriptor_length") = descLen
      spliceDescriptors = spliceDescriptors :+ spliceDescriptor
    }
    spliceDescriptors
  }

  /**
   * This method is responsible for parsing of "availDescriptor" part of the message.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded "availDescriptor" data.
   */
  private def parseAvailDescriptor(bitString: BitString): mutable.Map[String, Any] = {
    val availDescriptor = mutable.Map[String, Any]()
    availDescriptor("identifier") = bitString.read[Long](32)
    availDescriptor("providerAvailId") = bitString.read[Long](32)
    availDescriptor
  }

  /**
   * This method is responsible for parsing of "segmentationDescriptor" part of the message.
   *
   * @param bitString string of bits in BitString wrapper.
   * @return decoded "segmentationDescriptor" data.
   */
  private def parseSegmentationDescriptor(bitString: BitString): mutable.Map[String, Any] = {
    val segmentationDescriptor = mutable.Map[String, Any]()

    segmentationDescriptor("identifier") = bitString.read[Long](32)
    segmentationDescriptor("segmentation_event_id") = bitString.read[Long](32)
    segmentationDescriptor("segmentation_event_cancel_indicator") = bitString.read[Boolean](1)

    // next 7 bits reserved
    bitString.pos += 7

    if (segmentationDescriptor("segmentation_event_cancel_indicator") == false) {
      segmentationDescriptor("program_segmentation_flag") = bitString.read[Boolean](1)
      segmentationDescriptor("segmentation_duration_flag") = bitString.read[Boolean](1)
      segmentationDescriptor("delivery_not_restricted_flag") = bitString.read[Boolean](1)
      if (segmentationDescriptor("delivery_not_restricted_flag") == false) {
        segmentationDescriptor("web_delivery_allowed_flag") = bitString.read[Boolean](1)
        segmentationDescriptor("no_regional_blackout_flag") = bitString.read[Boolean](1)
        segmentationDescriptor("archive_allowed_flag") = bitString.read[Boolean](1)
        segmentationDescriptor("device_restrictions") = bitString.read[Int](2)
      } else {
        bitString.pos += 5
      }
      if (segmentationDescriptor("program_segmentation_flag") == false) {
        val componentCount = bitString.read[Int](8)
        var components = Seq[mutable.Map[String, Any]]()
        for (x <- 0 until componentCount) {
          val component = mutable.Map[String, Any]()
          component("tag") = bitString.read[Int](8)
          bitString.pos += 7
          component("ptsOffset") = bitString.read[Long](33)
          components = components :+ component
        }
        segmentationDescriptor("components") = components
        segmentationDescriptor("component_count") = componentCount
      }
      if (segmentationDescriptor("segmentation_duration_flag") == true) {
        segmentationDescriptor("segmentation_duration") = bitString.read[Long](40)
      }
      segmentationDescriptor("segmentation_upid_type") = bitString.read[Int](8)
      val upidLen = bitString.read[Int](8)
      segmentationDescriptor("segmentation_upid_length") = upidLen
      segmentationDescriptor("segmentation_upid") = bitString.read[AnyRef](upidLen * 8)
      segmentationDescriptor("segment_type_id") = bitString.read[Int](8)
      segmentationDescriptor("segment_num") = bitString.read[Int](8)
      segmentationDescriptor("segments_expected") = bitString.read[Int](8)
    }
    segmentationDescriptor
  }
}
