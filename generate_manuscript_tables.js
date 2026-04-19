// generate_manuscript_tables.js
// Breast Augmentation Ratio Analysis — Manuscript Tables
// Keck School of Medicine, USC | Casandra Serafin
//
// Reads outputs/tables/manuscript_table_data.json and generates
// outputs/manuscript_tables.docx
//
// Run: node outputs/generate_manuscript_tables.js

const {
  Document, Packer, Paragraph, TextRun, Table, TableRow, TableCell,
  AlignmentType, HeadingLevel, BorderStyle, WidthType, ShadingType,
  VerticalAlign, PageBreak, PageNumber, Footer, PageOrientation
} = require('docx');
const fs = require('fs');
const path = require('path');

// ── Load table data ──────────────────────────────────────────────────────────
const dataPath = path.join(__dirname, 'outputs', 'tables', 'manuscript_table_data.json');
const data = JSON.parse(fs.readFileSync(dataPath, 'utf8'));

// ── Style constants ──────────────────────────────────────────────────────────
const FONT       = "Times New Roman";
const FONT_SIZE  = 20; // half-points = 10pt
const HEADER_SIZE = 20;
const TITLE_SIZE  = 22; // 11pt
const PAGE_W     = 12240; // DXA — US Letter 8.5"
const PAGE_H     = 15840;
const MARGIN     = 1080; // 0.75" margins for wide tables
const CONTENT_W  = PAGE_W - (MARGIN * 2); // 10,080 DXA

const GRAY_HEADER = "E8E8E8";
const GRAY_STRIPE = "F5F5F5";
const BLACK       = "000000";
const WHITE       = "FFFFFF";

const thinBorder = { style: BorderStyle.SINGLE, size: 4, color: "AAAAAA" };
const thickBorder = { style: BorderStyle.SINGLE, size: 8, color: "000000" };
const noBorder   = { style: BorderStyle.NONE,   size: 0, color: "FFFFFF" };
const allThin    = { top: thinBorder,  bottom: thinBorder,
                     left: thinBorder, right: thinBorder };
const topThick   = { top: thickBorder, bottom: thinBorder,
                     left: noBorder,   right: noBorder };
const bottomThick = { top: noBorder,  bottom: thickBorder,
                      left: noBorder, right: noBorder };
const noSides    = { top: noBorder, bottom: noBorder,
                     left: noBorder, right: noBorder };

// ── Helper: text run ─────────────────────────────────────────────────────────
function txt(text, opts = {}) {
  return new TextRun({
    text:  text ?? "—",
    font:  FONT,
    size:  opts.size  ?? FONT_SIZE,
    bold:  opts.bold  ?? false,
    italics: opts.italic ?? false,
    color: opts.color ?? BLACK
  });
}

// ── Helper: paragraph ────────────────────────────────────────────────────────
function para(children, opts = {}) {
  const runs = Array.isArray(children)
    ? children
    : [txt(children, opts)];
  return new Paragraph({
    alignment: opts.align ?? AlignmentType.LEFT,
    spacing:   { before: opts.spaceBefore ?? 0, after: opts.spaceAfter ?? 60 },
    children:  runs
  });
}

// ── Helper: heading paragraph ────────────────────────────────────────────────
function heading(text, level = 1) {
  return new Paragraph({
    heading: level === 1 ? HeadingLevel.HEADING_1 : HeadingLevel.HEADING_2,
    spacing: { before: 240, after: 120 },
    children: [new TextRun({ text, font: FONT, size: TITLE_SIZE, bold: true })]
  });
}

// ── Helper: table title ──────────────────────────────────────────────────────
function tableTitle(text, note = null) {
  const items = [
    new Paragraph({
      spacing: { before: 300, after: 60 },
      children: [new TextRun({ text, font: FONT, size: FONT_SIZE, bold: true })]
    })
  ];
  if (note) {
    items.push(new Paragraph({
      spacing: { before: 0, after: 120 },
      children: [
        new TextRun({ text: "Note. ", font: FONT, size: FONT_SIZE, italics: true }),
        new TextRun({ text: note, font: FONT, size: FONT_SIZE })
      ]
    }));
  }
  return items;
}

// ── Helper: cell ─────────────────────────────────────────────────────────────
function cell(text, opts = {}) {
  const isHeader = opts.header ?? false;
  const cellW    = opts.width  ?? 1000;
  return new TableCell({
    width:   { size: cellW, type: WidthType.DXA },
    borders: opts.borders ?? allThin,
    shading: {
      fill: isHeader ? GRAY_HEADER : (opts.shade ? GRAY_STRIPE : WHITE),
      type: ShadingType.CLEAR
    },
    verticalAlign: VerticalAlign.CENTER,
    margins: { top: 80, bottom: 80, left: 120, right: 120 },
    children: [new Paragraph({
      alignment: opts.align ?? AlignmentType.LEFT,
      children:  [txt(text ?? "—", { bold: isHeader, size: FONT_SIZE })]
    })]
  });
}

// ── Helper: build generic data table from array of objects ───────────────────
function buildDataTable(rows, colDefs, shade = true) {
  // colDefs: [{ key, label, width, align }]
  const totalW = colDefs.reduce((s, c) => s + c.width, 0);

  const headerRow = new TableRow({
    tableHeader: true,
    children: colDefs.map(c =>
      cell(c.label, { header: true, width: c.width, align: c.align ?? AlignmentType.LEFT })
    )
  });

  const dataRows = rows.map((row, i) =>
    new TableRow({
      children: colDefs.map(c =>
        cell(row[c.key] ?? "—", {
          width: c.width,
          shade: shade && i % 2 === 1,
          align: c.align ?? AlignmentType.LEFT
        })
      )
    })
  );

  return new Table({
    width: { size: totalW, type: WidthType.DXA },
    columnWidths: colDefs.map(c => c.width),
    rows: [headerRow, ...dataRows]
  });
}

// ── Helper: page break ───────────────────────────────────────────────────────
function pageBreak() {
  return new Paragraph({
    children: [new TextRun({ break: 1 })]
  });
}

// ── Helper: spacer ───────────────────────────────────────────────────────────
function spacer(before = 120) {
  return new Paragraph({ spacing: { before, after: 0 }, children: [] });
}

// ── Helper: footnote paragraph ───────────────────────────────────────────────
function footnote(text) {
  return new Paragraph({
    spacing: { before: 60, after: 60 },
    children: [
      new TextRun({ text: "Abbreviations: ", font: FONT, size: FONT_SIZE, italics: true }),
      new TextRun({ text, font: FONT, size: FONT_SIZE })
    ]
  });
}

// =============================================================================
// BUILD DOCUMENT SECTIONS
// =============================================================================

const sections = [];

// ── Cover / Title ─────────────────────────────────────────────────────────────
sections.push(
  new Paragraph({ spacing: { before: 1440, after: 240 }, alignment: AlignmentType.CENTER,
    children: [new TextRun({ text: "Quantifying Aesthetic Outcomes in Breast Augmentation",
      font: FONT, size: 28, bold: true })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 0, after: 120 },
    children: [new TextRun({ text: "Manuscript Tables", font: FONT, size: 24, bold: true })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 0, after: 120 },
    children: [new TextRun({ text: "Casandra Serafin, M.S. Biostatistics Candidate",
      font: FONT, size: FONT_SIZE })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 0, after: 120 },
    children: [new TextRun({ text: "Keck School of Medicine, University of Southern California",
      font: FONT, size: FONT_SIZE })] }),
  new Paragraph({ alignment: AlignmentType.CENTER, spacing: { before: 0, after: 480 },
    children: [new TextRun({ text: `Generated: ${new Date().toLocaleDateString('en-US', {
      year: 'numeric', month: 'long', day: 'numeric' })}`,
      font: FONT, size: FONT_SIZE, italics: true })] }),
  pageBreak()
);

// ── TABLE 1: Demographics ─────────────────────────────────────────────────────
sections.push(
  ...tableTitle(
    "Table 1. Rater Characteristics by Cohort",
    "Data shown as n (%). — indicates item not collected for that cohort. " +
    "CI = confidence interval; ICC = intraclass correlation coefficient."
  )
);

// Surgeon demographics
sections.push(
  new Paragraph({ spacing: { before: 120, after: 60 },
    children: [new TextRun({ text: "Surgeon Cohort (N = 78)",
      font: FONT, size: FONT_SIZE, bold: true })] }),
  buildDataTable(data.table1_surg, [
    { key: "Variable", label: "Variable",          width: 2400 },
    { key: "Category", label: "Category",          width: 3600 },
    { key: "Surgeon (n = 78)", label: "n (%)",     width: 1800 }
  ])
);

sections.push(spacer(180));

// Lay demographics
sections.push(
  new Paragraph({ spacing: { before: 120, after: 60 },
    children: [new TextRun({ text: "Lay Person Cohort (N = 243)",
      font: FONT, size: FONT_SIZE, bold: true })] }),
  buildDataTable(data.table1_lay, [
    { key: "Variable", label: "Variable",              width: 2400 },
    { key: "Category", label: "Category",              width: 3600 },
    { key: "Lay Person (n = 243)", label: "n (%)",     width: 1800 }
  ]),
  pageBreak()
);

// ── TABLE 2A: Aesthetic Ratings ───────────────────────────────────────────────
const surgKey2a = Object.keys(data.table2a[0]).find(k => k.includes("Surgeon"));
const layKey2a  = Object.keys(data.table2a[0]).find(k => k.includes("Lay"));

sections.push(
  ...tableTitle(
    "Table 2A. Distribution of Aesthetic Ratings by Cohort",
    "OR = odds ratio. Ratings: 1 = Very Unattractive, 2 = Unattractive, " +
    "3 = Neutral, 4 = Attractive, 5 = Very Attractive. Binary outcome: " +
    "high aesthetic = rating ≥ 4."
  ),
  buildDataTable(data.table2a, [
    { key: "Label",  label: "Aesthetic Rating", width: 3200 },
    { key: surgKey2a, label: "Surgeon, n (%)", width: 2880,
      align: AlignmentType.CENTER },
    { key: layKey2a,  label: "Lay Person, n (%)", width: 2880,
      align: AlignmentType.CENTER }
  ]),
  spacer(240)
);

// ── TABLE 2B: Naturalness Ratings ─────────────────────────────────────────────
const surgKey2b = Object.keys(data.table2b[0]).find(k => k.includes("Surgeon"));
const layKey2b  = Object.keys(data.table2b[0]).find(k => k.includes("Lay"));

sections.push(
  ...tableTitle(
    "Table 2B. Distribution of Naturalness Ratings by Cohort",
    "Ratings: 1 = Very Unnatural, 2 = Unnatural, 3 = Neutral, " +
    "4 = Natural, 5 = Very Natural. Binary outcome: high naturalness = rating ≥ 4."
  ),
  buildDataTable(data.table2b, [
    { key: "Label",   label: "Naturalness Rating", width: 3200 },
    { key: surgKey2b, label: "Surgeon, n (%)",     width: 2880,
      align: AlignmentType.CENTER },
    { key: layKey2b,  label: "Lay Person, n (%)",  width: 2880,
      align: AlignmentType.CENTER }
  ]),
  pageBreak()
);

// ── TABLES 3A-3F: Model Results ───────────────────────────────────────────────
const modelTableDefs = [
  { key: "table3a", title: "Table 3A. Univariate Upper Proportion Models for Aesthetic Ratings by Cohort" },
  { key: "table3b", title: "Table 3B. Univariate Upper Proportion Models for Naturalness Ratings by Cohort" },
  { key: "table3c", title: "Table 3C. Univariate Post-Operative Ratio Models for Aesthetic Ratings by Cohort" },
  { key: "table3d", title: "Table 3D. Univariate Post-Operative Ratio Models for Naturalness Ratings by Cohort" },
  { key: "table3e", title: "Table 3E. Univariate Ratio Difference Models for Aesthetic Ratings by Cohort" },
  { key: "table3f", title: "Table 3F. Univariate Ratio Difference Models for Naturalness Ratings by Cohort" }
];

const modelNote = "OR = odds ratio; CI = confidence interval. " +
  "Models fitted using binary logistic mixed effects regression with cross-classified " +
  "random intercepts for image (PatientID) and rater. " +
  "All predictors standardised prior to modelling. " +
  "Bold p-values indicate statistical significance (p < 0.05). " +
  "Cumulative odds ratios reported for ordinal models.";

modelTableDefs.forEach((def, i) => {
  const tableData = data[def.key];
  if (!tableData || tableData.length === 0) return;

  sections.push(
    ...tableTitle(def.title, i === 0 ? modelNote : null),
    buildDataTable(tableData, [
      { key: "Term",       label: "Characteristic",       width: 3200 },
      { key: "Surg_OR_CI", label: "Surgeon OR (95% CI)", width: 2160,
        align: AlignmentType.CENTER },
      { key: "Surg_P",     label: "p-value",             width: 960,
        align: AlignmentType.CENTER },
      { key: "Lay_OR_CI",  label: "Lay Person OR (95% CI)", width: 2160,
        align: AlignmentType.CENTER },
      { key: "Lay_P",      label: "p-value",             width: 960,
        align: AlignmentType.CENTER }
    ])
  );

  if (i < modelTableDefs.length - 1) sections.push(spacer(240));
  else sections.push(pageBreak());
});

// ── TABLE 4: ICC ─────────────────────────────────────────────────────────────
sections.push(
  ...tableTitle(
    "Table 4. Intraclass Correlation Coefficients (ICC) by Outcome, Type, and Cohort",
    "ICC computed from null intercept binary logistic mixed models using the latent-variable " +
    "formula: ICC = σ²_RE / (σ²_RE + π²/3), where π²/3 ≈ 3.29 is the logistic residual variance. " +
    "Image ICC reflects agreement on which images look better; " +
    "Rater ICC reflects consistency of individual raters across images."
  ),
  buildDataTable(data.table4, [
    { key: "ICC Type",     label: "ICC Type",     width: 2160 },
    { key: "Outcome",      label: "Outcome",      width: 2160 },
    { key: "Surgeon ICC",  label: "Surgeon ICC",  width: 2520,
      align: AlignmentType.CENTER },
    { key: "Lay ICC",      label: "Lay ICC",      width: 2520,
      align: AlignmentType.CENTER }
  ]),
  pageBreak()
);

// ── TABLE 5: Concordance ──────────────────────────────────────────────────────
sections.push(
  ...tableTitle(
    "Table 5. Concordance Between Aesthetic and Naturalness Ratings by Cohort",
    "OR = odds ratio; CI = confidence interval. " +
    "High aesthetic = rating ≥ 4; High naturalness = rating ≥ 4. " +
    "OR represents the odds of a high naturalness rating given a high aesthetic rating " +
    "compared to a low aesthetic rating."
  ),
  buildDataTable(data.table5, [
    { key: "Cohort",               label: "Cohort",               width: 1080 },
    { key: "Aesthetic Category",   label: "Aesthetic Category",   width: 2160 },
    { key: "Low Naturalness",      label: "Low Naturalness",      width: 1440,
      align: AlignmentType.CENTER },
    { key: "High Naturalness",     label: "High Naturalness",     width: 1440,
      align: AlignmentType.CENTER },
    { key: "Row Total",            label: "Row Total",            width: 1080,
      align: AlignmentType.CENTER },
    { key: "% High Naturalness",   label: "% High Naturalness",   width: 1200,
      align: AlignmentType.CENTER },
    { key: "Odds Ratio (95% CI)",  label: "OR (95% CI)",          width: 1800,
      align: AlignmentType.CENTER },
    { key: "p-value",              label: "p-value",              width: 840,
      align: AlignmentType.CENTER }
  ]),
  pageBreak()
);

// ── SUPPLEMENTAL TABLE S1: Turning Points ────────────────────────────────────
sections.push(
  ...tableTitle(
    "Supplemental Table S1. Estimated Turning Points from Significant Quadratic Models",
    "Turning point = x = −b/(2a) where b is the linear coefficient and a is the quadratic " +
    "coefficient from the unadjusted binary logistic mixed model. " +
    "Only shown for models where the quadratic term reached statistical significance (p < 0.05). " +
    "Upper proportion was standardised prior to modelling; turning points are back-transformed " +
    "to the original ratio scale. " +
    "The observed range of upper proportion in this sample was approximately 0.46 to 0.73."
  ),
  buildDataTable(data.tableS1, [
    { key: "Rater Group",             label: "Rater Group",          width: 1440 },
    { key: "Outcome",                 label: "Outcome",              width: 1440 },
    { key: "Curve Shape",             label: "Curve Shape",          width: 2880 },
    { key: "Turning Point",           label: "Turning Point",        width: 1440,
      align: AlignmentType.CENTER },
    { key: "Within Observed Range",   label: "Within Range",         width: 1440,
      align: AlignmentType.CENTER },
    { key: "Quadratic p-value",       label: "Quadratic p-value",    width: 1440,
      align: AlignmentType.CENTER }
  ])
);

// =============================================================================
// ASSEMBLE AND WRITE DOCUMENT
// =============================================================================

const doc = new Document({
  styles: {
    default: {
      document: {
        run: { font: FONT, size: FONT_SIZE }
      }
    },
    paragraphStyles: [
      {
        id: "Heading1", name: "Heading 1", basedOn: "Normal",
        run: { size: TITLE_SIZE, bold: true, font: FONT },
        paragraph: { spacing: { before: 240, after: 120 }, outlineLevel: 0 }
      }
    ]
  },
  sections: [{
    properties: {
      page: {
        size:   { width: PAGE_W, height: PAGE_H },
        margin: { top: MARGIN, right: MARGIN, bottom: MARGIN, left: MARGIN }
      }
    },
    footers: {
      default: new Footer({
        children: [new Paragraph({
          alignment: AlignmentType.CENTER,
          children: [
            new TextRun({ text: "Serafin — Breast Augmentation Outcomes — Page ",
              font: FONT, size: 18 }),
            new TextRun({ children: [PageNumber.CURRENT], font: FONT, size: 18 })
          ]
        })]
      })
    },
    children: sections
  }]
});

const outPath = path.join(__dirname, 'outputs', 'manuscript_tables.docx');
Packer.toBuffer(doc).then(buffer => {
  fs.writeFileSync(outPath, buffer);
  console.log(`✓ Saved: ${outPath}`);
});
