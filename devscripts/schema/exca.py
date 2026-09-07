"""Tiny helper library to build .excalidraw files programmatically."""
import json, random

# --- palette (same one the existing schemas use) -------------------------------------------------
BLACK = "#1e1e1e"
GREY = "#5c5f66"
LIGHTGREY = "#adb5bd"
BLUE = "#1971c2"; BLUE_BG = "#e7f2fd"
GREEN = "#2f9e44"; GREEN_BG = "#e9f9ec"
RED = "#e03131"; RED_BG = "#ffeded"
ORANGE = "#f08c00"; ORANGE_BG = "#fff7e0"
VIOLET = "#6741d9"; VIOLET_BG = "#e9e2ff"

FONT = 5  # Nunito / normal sans

_seed = random.Random(20260828)
_counter = [0]
_index = [0]

# Excalidraw orders elements with fractional indices; "a00", "a01", ... is enough here.
_ALPHA = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


def _next_index():
    n = _index[0]
    _index[0] += 1
    return "a" + _ALPHA[n // len(_ALPHA)] + _ALPHA[n % len(_ALPHA)]


def _base(kind, x, y, w, h, stroke, bg, **kw):
    _counter[0] += 1
    el = {
        "id": "%s%04d" % (kind[:4], _counter[0]),
        "type": kind,
        "x": x, "y": y, "width": w, "height": h,
        "angle": 0,
        "strokeColor": stroke,
        "backgroundColor": bg,
        "fillStyle": "solid",
        "strokeWidth": 2,
        "strokeStyle": "solid",
        "roughness": 1,
        "opacity": 100,
        "groupIds": [],
        "frameId": None,
        "index": _next_index(),
        "roundness": None,
        "seed": _seed.randint(1, 2 ** 31),
        "version": 1,
        "versionNonce": _seed.randint(1, 2 ** 31),
        "isDeleted": False,
        "boundElements": [],
        "updated": 1787905195780,
        "link": None,
        "locked": False,
    }
    el.update(kw)
    return el


ELEMENTS = []


def add(el):
    ELEMENTS.append(el)
    return el


# --- text ----------------------------------------------------------------------------------------
def text_size(s, size):
    lines = s.split("\n")
    w = max(len(l) for l in lines) * size * 0.545
    h = len(lines) * size * 1.25
    return w, h


def T(x, y, s, size=13, color=GREY, align="left"):
    w, h = text_size(s, size)
    return add(_base("text", x, y, w, h, color, "transparent",
                     text=s, originalText=s, fontSize=size, fontFamily=FONT,
                     textAlign=align, verticalAlign="top", containerId=None,
                     autoResize=True, lineHeight=1.25))


# --- shapes --------------------------------------------------------------------------------------
def R(x, y, w, h, color=GREY, bg="transparent", dashed=False, sharp=False):
    el = _base("rectangle", x, y, w, h, color, bg,
               roundness=None if sharp else {"type": 3})
    if dashed:
        el["strokeStyle"] = "dashed"
    return add(el)


def A(points, color=BLACK, dashed=False, start_head=None, end_head="arrow", width=2):
    x0, y0 = points[0]
    rel = [[px - x0, py - y0] for px, py in points]
    w = max(p[0] for p in rel) - min(p[0] for p in rel)
    h = max(p[1] for p in rel) - min(p[1] for p in rel)
    el = _base("arrow", x0, y0, w, h, color, "transparent",
               roundness={"type": 2}, points=rel, lastCommittedPoint=None,
               startBinding=None, endBinding=None,
               startArrowhead=start_head, endArrowhead=end_head, elbowed=False)
    el["strokeWidth"] = width
    if dashed:
        el["strokeStyle"] = "dashed"
    return add(el)


def LINE(points, color=LIGHTGREY, dashed=False, width=1):
    el = A(points, color=color, dashed=dashed, end_head=None, width=width)
    el["type"] = "line"
    el.pop("elbowed", None)
    return el


# --- composite: a titled box -----------------------------------------------------------------
def box(x, y, w, title, lines=(), color=GREY, bg="transparent", h=None,
        title_size=14, line_size=12, dashed=False, pad=12):
    """Draw a rectangle with a coloured title and grey body lines. Returns the bottom y."""
    body = "\n".join(lines)
    ty = y + pad - 2
    inner = pad
    height = pad + (title_size * 1.25 if title else 0)
    if body:
        height += 6 + len(body.split("\n")) * line_size * 1.25
    height += pad
    if h is not None:
        height = h
    R(x, y, w, height, color, bg, dashed=dashed)
    if title:
        T(x + inner, ty, title, title_size, color)
        ty += title_size * 1.25 + 6
    if body:
        T(x + inner, ty, body, line_size, GREY)
    return y + height


def table(x, y, widths, header, rows, color=GREY, bg="transparent",
          head_size=12, row_size=12, rowh=None, pad=8):
    """Simple table: header row + body rows. Returns bottom y."""
    rowh = rowh or row_size * 1.25 + 10
    total_w = sum(widths)
    headh = head_size * 1.25 + 10
    height = headh + rowh * len(rows)
    R(x, y, total_w, height, color, bg)
    cx = x
    for i, c in enumerate(header):
        T(cx + pad, y + 6, c, head_size, color)
        cx += widths[i]
    LINE([(x, y + headh), (x + total_w, y + headh)], color=color)
    for j, row in enumerate(rows):
        ry = y + headh + rowh * j
        if j:
            LINE([(x, ry), (x + total_w, ry)], color=LIGHTGREY)
        cx = x
        for i, c in enumerate(row):
            T(cx + pad, ry + 5, c, row_size, GREY if i else BLACK)
            cx += widths[i]
    cx = x
    for wcol in widths[:-1]:
        cx += wcol
        LINE([(cx, y), (cx, y + height)], color=LIGHTGREY)
    return y + height


def panel(x, y, w, num_title, subtitle=None):
    """Panel heading. Returns the y where the panel content may start."""
    T(x, y, num_title, 20, BLACK)
    y += 26
    if subtitle:
        T(x, y, subtitle, 13, GREY)
        y += 6 + len(subtitle.split("\n")) * 13 * 1.25
    LINE([(x, y + 6), (x + w, y + 6)], color=LIGHTGREY)
    return y + 22


def dump(path, elements=None):
    doc = {
        "type": "excalidraw",
        "version": 2,
        "source": "https://marketplace.visualstudio.com/items?itemName=pomdtr.excalidraw-editor",
        "elements": elements if elements is not None else ELEMENTS,
        "appState": {"gridSize": 20, "gridStep": 5, "gridModeEnabled": False,
                     "viewBackgroundColor": "#ffffff"},
        "files": {},
    }
    with open(path, "w", encoding="utf-8") as f:
        # Même format que l'extension Excalidraw de VS Code : indentation de 2, UTF-8 brut.
        json.dump(doc, f, ensure_ascii=False, indent=2)
    return doc
