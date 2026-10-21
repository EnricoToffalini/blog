// Post-render: make link previews (WhatsApp, social networks) robust.
// - Empty og/twitter descriptions (Quarto 1.7 leaves them empty when the
//   first paragraph has links or formatting) get the first paragraph.
// - Preview images that are too heavy for WhatsApp, or whose URL contains
//   spaces, are replaced by the blog card.
// Descriptions and images set in the front matter are otherwise untouched.

const SITE = "https://enricotoffalini.github.io/blog/";
const CARD = SITE + "images/social-card.jpg";
const CARD_ALT = "toffeeblog, il blog di Enrico Toffalini";
const MAX_BYTES = 500 * 1024;

const outDir = Deno.env.get("QUARTO_PROJECT_OUTPUT_DIR") ?? "docs";
const files = (Deno.env.get("QUARTO_PROJECT_OUTPUT_FILES") ?? "")
  .split(/\r?\n/).filter((f) => f.endsWith(".html"));

const decode = (s: string) => s
  .replace(/&nbsp;/g, " ").replace(/&lt;/g, "<").replace(/&gt;/g, ">")
  .replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&amp;/g, "&");
const encode = (s: string) => s
  .replace(/&/g, "&amp;").replace(/"/g, "&quot;")
  .replace(/</g, "&lt;").replace(/>/g, "&gt;");

function firstParagraph(html: string): string | undefined {
  const main = html.match(/<main class="content[^>]*>([\s\S]*?)<\/main>/);
  if (!main) return undefined;
  const body = main[1].replace(/<header[\s\S]*?<\/header>/, "");
  for (const p of body.matchAll(/<p>([\s\S]*?)<\/p>/g)) {
    const text = decode(p[1].replace(/<[^>]+>/g, "")).replace(/\s+/g, " ").trim();
    if (text.length > 40) return text;
  }
}

function cut(s: string, n = 200): string {
  const chars = [...s];
  if (chars.length <= n) return s;
  return chars.slice(0, n).join("").replace(/\s+\S*$/, "") + "…";
}

function badImage(url: string): boolean {
  if (url.includes(" ")) return true;
  if (!url.startsWith(SITE)) return false;
  try {
    return Deno.statSync(`${outDir}/${decodeURI(url.slice(SITE.length))}`).size > MAX_BYTES;
  } catch {
    return false;
  }
}

for (const file of files) {
  let html = Deno.readTextFileSync(file);
  const before = html;

  const emptyDesc = /<meta (property="og|name="twitter):description" content="">/g;
  if (emptyDesc.test(html)) {
    const text = firstParagraph(html);
    if (text) {
      const value = encode(cut(text));
      html = html.replace(emptyDesc, (_m, attr) => `<meta ${attr}:description" content="${value}">`);
    }
  }

  const image = html.match(/<meta property="og:image" content="([^"]*)">/);
  if (image && badImage(decode(image[1]))) {
    html = html
      .replace(/<meta (property="og|name="twitter):image" content="[^"]*">/g,
        (_m, attr) => `<meta ${attr}:image" content="${CARD}">`)
      .replace(/<meta (property="og|name="twitter):image:alt" content="[^"]*">\n?/g, "")
      .replace(/<meta (property="og:image:|name="twitter:image-)(width|height)" content="[^"]*">\n?/g, "")
      .replace(/(<meta property="og:image" content="[^"]*">)/,
        `$1\n<meta property="og:image:alt" content="${CARD_ALT}">`)
      .replace(/(<meta name="twitter:image" content="[^"]*">)/,
        `$1\n<meta name="twitter:image:alt" content="${CARD_ALT}">`);
  }

  if (html !== before) Deno.writeTextFileSync(file, html);
}
