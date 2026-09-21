-- Makes the <span style="color:red"> markers of this vignette actually red in the Word export.
--
-- Pandoc parses those spans into Span elements (native_spans), but its docx writer has no use for
-- a `style` attribute and silently drops it, so the review copy came out entirely black. The one
-- thing the docx writer does honour on a Span is `custom-style`, which it turns into a character
-- style reference - hence reference-red.docx, which carries the two character styles used below.
--
-- Inline code gets its own treatment: the writer always stamps `Code` with VerbatimChar, which
-- wins over the red of the surrounding span. Inside a red span it is rewritten as a plain span
-- carrying RedVerbatimChar, a style that is monospaced *and* red.
local function is_red(el)
  local style = el.attributes["style"]
  return style ~= nil and string.find(string.lower(style), "color%s*:%s*red") ~= nil
end

function Span(el)
  if not is_red(el) then
    return el
  end
  el.attributes["custom-style"] = "RedText"
  el.content = el.content:walk({
    Code = function(code)
      return pandoc.Span(
        { pandoc.Str(code.text) },
        pandoc.Attr("", {}, { ["custom-style"] = "RedVerbatimChar" })
      )
    end,
  })
  return el
end
