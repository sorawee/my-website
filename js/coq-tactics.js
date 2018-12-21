$(() => {
  const map = {}; // everything is implicitly 0 initially

  function contButton(contFunc, bound) {
    return e => {
      const dataTag = $(e.currentTarget).parent();
      const size = dataTag.data('size');
      const refid = dataTag.data('refid');
      if (!(refid in map)) map[refid] = 0;
      const currentIndex = map[refid];
      if ((bound >= 0 && currentIndex === bound) ||
          (bound < 0  && currentIndex === bound + size)) return;
      const contIndex = contFunc(currentIndex);
      map[refid] = contIndex;
      $(`#coq-box-${refid} .coq-pane.show-all`).css('display', 'none');
      $(`#coq-box-${refid} .coq-pane.show-${contIndex}`).css('display', 'block');

      $(`#coq-box-${refid} .coq-script .show-all`)
        .css('background-color', '');
      $(`#coq-box-${refid} .coq-script .show-${contIndex}`)
        .css('background-color', '#acf2bd');
    };
  }

  $('.next-button').click(contButton(x => x + 1, -1));
  $('.prev-button').click(contButton(x => x - 1, 0));
});
