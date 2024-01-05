HTMLWidgets.widget({

  name: 'geogebra',

  type: 'output',

  factory: function (el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function (input) {
        el.innerHTML = `<div id="geogebra"></div>`

        console.log("test");

        const params = {
          "code_geogebra": "graphing",
          "width": width,
          "height": height,
          "prerelease": false,
          "showToolBar": false,
          "borderColor": null,
          "showMenuBar": false,
          "showAlgebraInput": false,
          "customToolbar": "0 || 1",
          "showResetIcon": false,
          "enableLabelDrags": false,
          "enableShiftDragZoom": false,
          "enableRightClick": false,
          "capturingThreshold": null,
          "showToolBarHelp": false,
          "errorDialogsActive": false,
          "useBrowserForJS": false,
          "ggbBase64": input.ggb
        };

        const applet = new GGBApplet(params, true);
        applet.inject('geogebra');
        // window.addEventListener("load", function () {
        //   applet.inject('geogebra');
        // });

      },

      resize: function (width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});
