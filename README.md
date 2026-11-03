# Python Skin Listing Monitor

A small R Shiny app that helps a human reviewer triage online marketplace listings that
may be selling python skin products, by ranking listings that show **no proof of
origin** (e.g. no CITES permit or skin tag ID mentioned).

Built for the BL5314 (CITES and the Illegal Wildlife Trade) group project
**"Show Me the Source."**

> **A flag from this tool is not proof that a listing is illegal.** The app only
> sorts listings for a person to review; a human always makes the final call.

## What it does

1. **Collect** — load a CSV of marketplace listings (`id, platform, date, account, text, price`).
2. **Match keywords** — scans listing text for species names, product terms, sale
   cues, and proof-of-origin terms, in English, 中文, Malay, and Indonesian.
3. **Filter and rank** — scores each listing and assigns a priority:
   - **P1** — no proof of origin + strong sale cues
   - **P2** — no proof of origin
   - **P3** — not relevant, or proof is mentioned
4. **Decide and record** — a reviewer reads each flagged listing, picks a decision
   (escalate / review later / dismiss) and a reason; decisions are logged so the
   team can see how many flags were useful and improve the keyword lists over time.

Method follows Di Minin et al. (2019) and Xu et al. (2019).

## Run it

Requires R (4.2+) and the `shiny` package.

```r
install.packages("shiny")
shiny::runApp(".")
```

Without an uploaded CSV, the app loads `data/sample_listings.csv` — **14 fictional
test listings** used only to demonstrate the scoring logic.

## Files

- `app.R` — the Shiny app (UI + server + scoring logic)
- `data/sample_listings.csv` — fictional sample listings for demo purposes

## Customising

Keyword lists (species, product, sale, proof, weak-proof, exclude terms) can be
edited directly in `app.R`, or live in the app's **Keywords** tab while it's running
— useful for adding new slang or local-language terms as they come up.

## License

MIT — see [LICENSE](LICENSE).
