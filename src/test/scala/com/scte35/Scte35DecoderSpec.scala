package com.scte35

import org.scalatest.FunSuite

class Scte35DecoderSpec extends FunSuite {

  test("scte35 decode -> full messages") {
    val actual1 = Scte35Decoder.base64Decode("/DAlAAAAAAAAAP/wFAUAAAABf+/+LRQrAP4BI9MIAAEBAQAAfxV6SQ==")
    val actual2 = Scte35Decoder.base64Decode("/DAlAAAAAAAAAP/wFAUAAAPof+//SVqZrP4Ae5igAAEBAQAAQcfnVA==")
    val actual3 = Scte35Decoder.base64Decode("/DAlAAAAAAAAAP/wFAUAACtnf+/+s9z9LP4Ae5igAAEBAQAAwWSPdQ==")

    val expected1 = Some(
      Map(
        "protocol_version" -> 0,
        "splice_descriptors" -> List(),
        "section_syntax_indicator" -> false,
        "private" -> false,
        "cw_index" -> 0,
        "splice_command" -> Map(
          "out_of_network_indicator" -> true,
          "avail_num" -> 1,
          "splice_immediate_flag" -> false,
          "avails_expected" -> 1,
          "splice_event_cancel_indicator" -> false,
          "unique_program_id" -> 1,
          "duration_flag" -> true,
          "components" -> List(),
          "splice_time" -> Map(
            "time_specified_flag" -> true,
            "pts_time" -> 756296448L
          ),
          "program_splice_flag" -> true,
          "break_duration" -> Map(
            "auto_return" -> true,
            "duration" -> 19125000
          ),
          "splice_event_id" -> 1
        ),
        "tier" -> "fff",
        "splice_descriptor_loop_length" -> 0,
        "pts_adjustment" -> 0,
        "splice_command_type" -> 5,
        "section_length" -> 37,
        "encrypted_packet" -> false,
        "encryption_algorithm" -> 0,
        "splice_command_length" -> 20
      )
    )

    val expected2 = Some(
      Map(
        "protocol_version" -> 0,
        "splice_descriptors" -> List(),
        "section_syntax_indicator" -> false,
        "private" -> false,
        "cw_index" -> 0,
        "splice_command" -> Map(
          "out_of_network_indicator" -> true,
          "avail_num" -> 1,
          "splice_immediate_flag" -> false,
          "avails_expected" -> 1,
          "splice_event_cancel_indicator" -> false,
          "unique_program_id" -> 1,
          "duration_flag" -> true,
          "components" -> List(),
          "splice_time" -> Map(
            "time_specified_flag" -> true,
            "pts_time" -> 5525641644L
          ),
          "program_splice_flag" -> true,
          "break_duration" -> Map(
            "auto_return" -> true,
            "duration" -> 8100000
          ),
          "splice_event_id" -> 1000
        ),
        "tier" -> "fff",
        "splice_descriptor_loop_length" -> 0,
        "pts_adjustment" -> 0,
        "splice_command_type" -> 5,
        "section_length" -> 37,
        "encrypted_packet" -> false,
        "encryption_algorithm" -> 0,
        "splice_command_length" -> 20
      )
    )

    val expected3 = Some(
      Map("protocol_version" -> 0,
        "splice_descriptors" -> List(),
        "section_syntax_indicator" -> false,
        "private" -> false,
        "cw_index" -> 0,
        "splice_command" -> Map(
          "out_of_network_indicator" -> true,
          "avail_num" -> 1,
          "splice_immediate_flag" -> false,
          "avails_expected" -> 1,
          "splice_event_cancel_indicator" -> false,
          "unique_program_id" -> 1,
          "duration_flag" -> true,
          "components" -> List(),
          "splice_time" -> Map(
            "time_specified_flag" -> true,
            "pts_time" -> 3017604396L
          ),
          "program_splice_flag" -> true,
          "break_duration" -> Map(
            "auto_return" -> true,
            "duration" -> 8100000
          ),
          "splice_event_id" -> 11111
        ),
        "tier" -> "fff",
        "splice_descriptor_loop_length" -> 0,
        "pts_adjustment" -> 0,
        "splice_command_type" -> 5,
        "section_length" -> 37,
        "encrypted_packet" -> false,
        "encryption_algorithm" -> 0,
        "splice_command_length" -> 20
      )
    )

    assert(Some(actual1) === expected1)
    assert(Some(actual2) === expected2)
    assert(Some(actual3) === expected3)
  }
}
