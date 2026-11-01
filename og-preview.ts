// Post-render: make link previews (WhatsApp, social networks) consistent.
// - Each post uses its own card (<name>.og.png next to the source, made by
//   _og/make-cards.R), copied next to the page; without a card, the blog card.
// - Empty og/twitter descriptions (Quarto 1.7 leaves them empty when the
//   first paragraph has links or formatting) get the first paragraph.

const SITE = "https://enricotoffalini.github.io/blog/";
const CARD = SITE + "images/social-card.jpg";
const CARD_ALT = "toffeeblog, il blog di Enrico Toffalini";

// paths relative to the project folder, with forward slashes (also on Windows)
const cwd = Deno.cwd().replace(/\\/g, "/") + "/";
const relative = (f: string) => {
  f = f.replace(/\\/g, "/");
  return f.toLowerCase().startsWith(cwd.toLowerCase()) ? f.slice(cwd.length) : f;
};
const outDir = relative(Deno.env.get("QUARTO_PROJECT_OUTPUT_DIR") ?? "docs").replace(/\/$/, "");
const files = (Deno.env.get("QUARTO_PROJECT_OUTPUT_FILES") ?? "")
  .split(/\r?\n/).map(relative).filter((f) => f.startsWith(outDir + "/") && f.endsWith(".html"));

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

const exists = (f: string) => { try { return Deno.statSync(f).isFile; } catch { return false; } };

for (const file of files) {
  const rel = file.slice(outDir.length + 1);
  if (!rel.startsWith("posts/")) continue;
  let html = Deno.readTextFileSync(file);
  if (!html.includes('<meta property="og:')) continue;
  const before = html;

  // preview image: the post card, else the blog card
  const cardRel = rel.replace(/\.html$/, ".og.png");
  const sources = [".qmd", ".Qmd"].map((e) => rel.replace(/\.html$/, e));
  let image = CARD, alt = CARD_ALT, size = "";
  if (sources.some(exists) && exists(cardRel)) {
    Deno.copyFileSync(cardRel, `${outDir}/${cardRel}`);
    image = SITE + cardRel.split("/").map(encodeURIComponent).join("/");
    alt = decode(html.match(/<meta property="og:title" content="([^"]*)">/)?.[1] ?? CARD_ALT);
    size = "1200x630";
  }
  html = html
    .replace(/<meta (property="og:image:|name="twitter:image[:-])(alt|width|height)" content="[^"]*">\n?/g, "")
    .replace(/<meta (property="og|name="twitter):image" content="[^"]*">/g, (_m, attr) => {
      const tw = attr.startsWith("name");
      const tag = (k: string, v: string) => tw
        ? `<meta name="twitter:image${k === "alt" ? ":" : "-"}${k}" content="${v}">`
        : `<meta property="og:image:${k}" content="${v}">`;
      return [`<meta ${attr}:image" content="${image}">`, tag("alt", encode(alt)),
        ...(size ? [tag("width", "1200"), tag("height", "630")] : [])].join("\n");
    });

  // description: first paragraph when empty
  const emptyDesc = /<meta (property="og|name="twitter):description" content="">/g;
  if (emptyDesc.test(html)) {
    const text = firstParagraph(html);
    if (text) {
      const value = encode(cut(text));
      html = html.replace(emptyDesc, (_m, attr) => `<meta ${attr}:description" content="${value}">`);
    }
  }

  if (html !== before) Deno.writeTextFileSync(file, html);
}
