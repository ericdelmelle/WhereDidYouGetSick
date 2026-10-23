# Where Did You Get Sick? — FYS, Oct 23, 2026

One fictional case (Riverton) drives the lecture and the recitation. All maps and every number come from `R/riverton.R`, so editing a case there updates the slides, handouts and key together.

| File | What it is |
|---|---|
| `lecture.qmd` | Quarto revealjs deck (28 slides, ~55 min; speaker notes with timings, press `S`) |
| `recitation_student.qmd` | Student packet (Typst PDF): briefing, Rounds 1–4, record sheet |
| `recitation_ta_guide.qmd` | Facilitator guide for the TA, with the case key |
| `R/riverton.R` | Town geometry, the 23 cases, headline numbers (`nums`) |
| `R/maps.R` | ggplot2/sf map functions; `options(riverton.tm = )` scales map text |
| `make_figures.R` | Writes all maps as PNG to `figures/` |
| `fys.scss`, `_handout-style.typ` | Slide and handout styling |

Render from this folder: `quarto render lecture.qmd` (and the two recitation files). R packages: sf, ggplot2, patchwork. The PDFs use Typst (bundled with Quarto), so no LaTeX is needed.
