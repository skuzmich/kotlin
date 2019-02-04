package com.jakewharton.sdksearch.api.dac

expect interface DocumentationService {
  fun list(): List<Item>
}
