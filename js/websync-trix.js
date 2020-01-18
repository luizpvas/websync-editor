import * as loader from "./loader";

class WebsyncTrix extends HTMLElement {
  connectedCallback() {
    loader.load(
      [
        "https://cdnjs.cloudflare.com/ajax/libs/trix/1.2.1/trix-core.js",
        "https://cdnjs.cloudflare.com/ajax/libs/trix/1.2.1/trix.css"
      ],
      () => {
        let id = new Date().getTime();

        this.innerHTML = `
          <input id="${id}" type="hidden" name="content">
        `;

        let input = this.querySelector("input");
        input.value = this.getAttribute("data-html");

        this.insertAdjacentHTML(
          "beforeend",
          `<trix-editor input="${id}"></trix-editor>`
        );

        let editor = this.querySelector("trix-editor");

        editor.addEventListener("trix-change", ev => {
          this.dispatchEvent(
            new CustomEvent("trix-change", {
              detail: {
                html: input.value,
                text: editor.editor.getDocument().toString()
              }
            })
          );
        });

        editor.addEventListener("trix-selection-change", ev => {
          window.editor = editor.editor;
          let rect = editor.editor.getClientRectAtPosition(
            editor.editor.getSelectedRange()[0]
          );
          console.log("selection changed...", rect);
        });

        editor.focus();
      }
    );
  }

  disconnectedCallback() {
    console.log("disconnected!");
  }
}

window.customElements.define("websync-trix", WebsyncTrix);
