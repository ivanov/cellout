# cellout

## Rich notebook conversion

Cellout is both a command-line utility as well as a Haskell library for
operating on Jupyter notebooks. You can think of cellout as a translation layer
between different kinds of notebook formats. An additional goal of `cellout` is
to provide `nbconvert` functionality without a python runtime dependency.

Cellout was written to provide a solid foundation for experimenting with
notebook file formatsn. Integrating well with pandoc by providing readers has
been one of the motivating factors.

The haskell library contains a well-typed Jupyter notebook document
representation, as well as facilities for operating on notebooks (such as
filtering output, removing metadata, concatenating multple notebooks).


## Examples

```
$ cellout --clear-output Untitled314.ipynb
$
```

When there are no errors to report, cellout stays quiet, though you can pass the
`--verbose` flag to get informational output.


```
$ cellout --clear-output --verbose Untitled314.ipynb
Wrote Untitled314.ipynb
$
```

## Why cellout?

Personal context. I am writing cellout in part to have a mechanism for exploring
future notebook formats that is not tied down to the JSON `.ipynb` format. From
the beginning of nbconvert back in 2012, I've expressed the opinion in the past
that we should integrate with pandoc as much as we can and not have nbconvert do
so much of what it does, and all that was missing is someone sitting down and
learning more Haskell, which is what I've now started doing myself.

The JSON based `.ipynb` Jupyter's notebook format has been hugely successful, on
the one hand, because everything is in one file and it can act as a unit of
sharing, but it has also been a hindrance in other contexts. For example, the
JSON format impedes effective version control, and being a single-file monolith
means you can't load large notebooks quickly, since the entire `.ipynb` file needs
to be transferred before the javascript can render it. That's not a big deal for
short little notebooks, but in practice it has meant that no one writes long
notebooks, and it's a bummer that there's effectively a cap on how many plots
you pack into one.

At the last Project Jupyter all hands week-long meeting back in May we discussed
different requirements, trade-offs, features, limitations and pain points for
possible future versions of the notebook document format. So there are several
ideas that people have for addressing particular kinds of uses cases where
`.ipynb` file limitations end up a bottleneck.

As a Haskell library, cellout provides a well-defined in-memory representation
of a notebook and can be used to convert and translate between these future
formats. It will also allow us to integrate better with pandoc, and bring
nbconvert functionality in a standalone executable (no python dependencies).

## Visual summary
```
notebook* (* - or some notebook-compatible file format)
    |
    V
 cellout (filter/transform)
    |
    V
notebook*
```

In a more complex scenario, you can also use cellout as a reader for pandoc:

```
notebook*
    |
    V
 cellout (filter/transform)
    |
    V
 pandoc
    |
    V
one of the many output formats pandoc supports

```


## Why is cellout not a pandoc filter?

Pandoc filters operate on the pandoc abstract syntax tree (AST), which I think
would be "lossy" without some hacks. It's quite reasonable to convert a notebook
to a document, but to have that document preserve notions of cells, outputs,
etc, would create too much work for writing things like "clear the HTML-based
outputs" while operating on the pandoc AST.

## Formats
- [p] - partial
- [x] - fully supported
- [ ] - planned

- [p]  .ipynb (nbformat 4.2)
- [ ] [ipyaml](https://github.com/prabhuramachandran/ipyaml)
- [ ] [spinx-gallery](https://sphinx-gallery.readthedocs.io/en/latest/syntax.html)
- [ ] comment percent-percent format used by [spyder](https://docs.spyder-ide.org/editor.html#defining-code-cells)
- [ ] jupytext's extensions to the [percent format](https://github.com/mwouts/jupytext#the-percent-format)
- [ ] [ipymd](https://github.com/rossant/ipymd)

## Related projects

[nbconvert](https://nbconvert.readthedocs.io/en/latest/) - a python implementation of converting `.ipynb` to other formats.

> *Primarily, the `nbconvert` tool allows you to convert a Jupyter `.ipynb`
> notebook document file into another static format including HTML, LaTeX, PDF,
> Markdown, reStructuredText, and more. nbconvert can also add productivity to
> your workflow when used to execute notebooks programmatically.*

[ipyaml](https://github.com/prabhuramachandran/ipyaml) - IPython Notebooks as
YAML.

[jupytext](https://github.com/mwouts/jupytext) - provides text-editor friendly format conversion to/from notebooks.

[ipymd](https://github.com/rossant/ipymd) - markdown format for notebooks
(excludes outputs).

[nbconvert-vc](https://github.com/mdboom/nbconvert_vc) - The experimental
nbconvert plugin (to a YAML format) mentioned by [Mike Droettboom in his Jupyter
Notebooks and Version Control post](https://droettboom.com/blog/2018/01/18/diffable-jupyter-notebooks/).

[nbdime](https://nbdime.readthedocs.io/en/stable/) - diffing and merging in
Jupyter Notebooks.

[spinx-gallery](https://sphinx-gallery.readthedocs.io/en/latest/syntax.html) -
has a format of converting python scripts to notebooks and to Restructured Text
(`.rst`) with execution results included.


