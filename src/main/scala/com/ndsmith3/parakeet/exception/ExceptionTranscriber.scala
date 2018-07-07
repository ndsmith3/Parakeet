package com.ndsmith3.parakeet.exception

import com.ndsmith3.parakeet.exception._

object ExceptionTranscriber {
  def transcribe(exception: ParakeetException): String = exception.message
}
