# test-html.R

# html_escape ----
test_that("html_escape escapes the text-critical characters", {
  expect_equal(html_escape("a < b > c & d"), "a &lt; b &gt; c &amp; d")
})

test_that("html_escape escapes the ampersand it introduces only once", {
  expect_equal(html_escape("&lt;"), "&amp;lt;")
})

test_that("html_escape leaves quotes alone unless building an attribute", {
  expect_equal(html_escape("say \"hi\""), "say \"hi\"")
  expect_equal(
    html_escape("say \"hi\"", attribute = TRUE),
    "say &quot;hi&quot;"
  )
  expect_equal(html_escape("it's", attribute = TRUE), "it&#39;s")
})

test_that("html_escape returns a bare character", {
  expect_null(attributes(html_escape("x")))
})


# html_raw ----
test_that("html_raw embeds markup verbatim where text would be escaped", {
  expect_equal(
    as.vector(html_span(html_raw("<em>x</em>"))),
    "<span><em>x</em></span>"
  )
  expect_equal(
    as.vector(html_span("<em>x</em>")),
    "<span>&lt;em&gt;x&lt;/em&gt;</span>"
  )
})


# html_tag ----
test_that("html_tag renders a single text child inline", {
  expect_equal(as.vector(html_div("a")), "<div>a</div>")
})

test_that("html_tag renders an element child as an indented block", {
  # An element child always opens a block, so nesting stays visible.
  expect_equal(
    as.vector(html_div(html_span("a"))),
    "<div>\n  <span>a</span>\n</div>"
  )
})

test_that("html_tag renders several text children as a block", {
  expect_equal(as.vector(html_span("a", "b")), "<span>\n  a\n  b\n</span>")
})

test_that("html_tag indents each level of nesting", {
  expect_equal(
    as.vector(html_div(html_p(html_span("a")))),
    "<div>\n  <p>\n    <span>a</span>\n  </p>\n</div>"
  )
})

test_that("html_tag renders an empty element", {
  expect_equal(as.vector(html_div()), "<div></div>")
})

test_that("html_tag writes attributes in a fixed order and escapes them", {
  expect_equal(
    as.vector(html_div("x", class = "a\"b", style = "c<d", id = "e")),
    "<div id=\"e\" class=\"a&quot;b\" style=\"c&lt;d\">x</div>"
  )
})

test_that("html_tag drops NULL children", {
  # Lets an optional child be written as a conditional returning NULL.
  expect_equal(as.vector(html_div("a", NULL, "b")), "<div>\n  a\n  b\n</div>")
  expect_equal(as.vector(html_div(NULL)), "<div></div>")
})

test_that("html_tag flattens list children", {
  expect_equal(
    as.vector(html_ul(lapply(1:2, html_li))),
    "<ul>\n  <li>1</li>\n  <li>2</li>\n</ul>"
  )
  # A one-element list still holds an element, so it is still a block.
  expect_equal(
    as.vector(html_ul(lapply(1L, html_li))),
    "<ul>\n  <li>1</li>\n</ul>"
  )
})

test_that("html_tag indents a child's own line breaks along with it", {
  # Indentation tracks nesting depth even for pre-built markup, so a raw child
  # holding line breaks does not fall back to column zero.
  expect_equal(
    as.vector(html_ul(html_li(html_raw("x\ny")))),
    "<ul>\n  <li>x\n  y</li>\n</ul>"
  )
})

test_that("html_tag coerces non-character children", {
  expect_equal(as.vector(html_span(5L)), "<span>5</span>")
})

test_that("html_tag escapes text children but not built children", {
  expect_equal(
    as.vector(html_div(html_strong("a<b"), "c<d")),
    "<div>\n  <strong>a&lt;b</strong>\n  c&lt;d\n</div>"
  )
})

test_that("html_tag output composes through paste and html_raw", {
  # The pattern callers use to build a run of inline markup and pass it on as
  # one child: paste() drops the class, html_raw() restores it.
  expect_equal(
    as.vector(html_li(html_raw(paste(html_strong(5L), "numeric")))),
    "<li><strong>5</strong> numeric</li>"
  )
})


# constructors ----
test_that("the element constructors name their own tags", {
  expect_equal(as.vector(html_p("a")), "<p>a</p>")
  expect_equal(as.vector(html_strong("a")), "<strong>a</strong>")
  expect_equal(as.vector(html_li("a")), "<li>a</li>")
  expect_equal(as.vector(html_tag("section", "a")), "<section>a</section>")
})

test_that("built elements carry both classes, so nesting can tell them apart", {
  expect_s3_class(html_div("a"), "rtemis_html_element")
  expect_s3_class(html_div("a"), "rtemis_html")
  expect_s3_class(html_raw("<i>x</i>"), "rtemis_html")
  expect_false(inherits(html_raw("<i>x</i>"), "rtemis_html_element"))
})
