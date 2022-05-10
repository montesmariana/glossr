# glossr 0.5.1

- New option to add spacing above and below the glosses in PDF.

- Read Latex coding of "zero" for HTML. (TODO figure out for Word.)

- Fix the expex latex dependency for compatibility with Xelatex.

# glossr 0.5.0

- The default styling of the lines has no italics or bold but it is possible to define a consistent style (italics or bold) for each of the elements of a gloss. See `vignette('styling')`.

# glossr 0.4.0

- New and better error messages.

- The translation is Word output now spans as long as the content (it might be too long and the user might have to shorten the table).

- `add_gloss()` and `gloss_df()` have a new `output_format` argument to set a specific format.

# glossr 0.3.0

- Add `gloss_word()` function and utility functions to generate Word output using `flextable::flextable()`.

# glossr 0.2.0

- Create class `gloss_data` to read and manage data from glosses.
Now `as_gloss()` doesn't care about "parsed", "original" or any other other
argument names beyond "translation", "label" and "source". All other names will
be interpreted as lines to align (with a maximum of 3).

- A source can now be printed at the top of the gloss example.

- Translations are automatically surrounded by quotation marks. The actual quotation marks can be removed or replaced either by giving them as the `trans_quotes` argument to `as_gloss()` or by setting `options("glossr.trans.quotes" = "yourquotes")`.

- Adapt `gloss_pdf()` and `gloss_html()` to work with `gloss_data`.

- The LateX package used for pdf now is `exPex` instead of `gb4e`. No need to add `\noautomath` anymore.
