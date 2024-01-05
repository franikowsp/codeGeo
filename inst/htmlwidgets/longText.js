HTMLWidgets.widget({
  name: "longText",

  type: "output",

  factory: function (el, width, height) {
    // TODO: define shared variables for this instance

    return {
      renderValue: function (input) {
        el.innerHTML = `<div id="long-text" style="overflow-wrap: normal; font-size: 30px; display: block;">${input.text}</div>`;
        console.log(input);
      },

      resize: function (width, height) {
        // TODO: code to re-render the widget with a new size
      },
    };
  },
});
