$(() => {
  $('[data-toggle="tooltip"]').tooltip();

  MathJax.Hub.Config({
    tex2jax: {inlineMath: [['$','$']]},
    TeX: {
      Macros: {
        seq: ["{\\langle #1 \\rangle}", 1],
        set: ["{\\{ #1 \\}}", 1],
        setof: ["{\\{ #1 \\ : \\ #2 \\}}", 2],
        N: "{\\mathbb N}",
        R: "{\\mathbb R}",
        Z: "{\\mathbb Z}",
        Q: "{\\mathbb Q}",
        lam: ["{\\lambda #1.\ #2}", 2],
        floor: ["{\\lfloor #1 \\rfloor}", 1],
        ceil: ["{\\lceil #1 \\rceil}", 1],
      }
    },
    menuSettings: {
      autocollapse: true
    }
  });
});
