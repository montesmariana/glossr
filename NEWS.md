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