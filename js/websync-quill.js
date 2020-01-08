import * as loader from "./loader";

class WebsyncQuill extends HTMLElement {
  connectedCallback() {
    loader.load(
      [
        "https://cdn.quilljs.com/1.3.6/quill.bubble.css",
        "https://cdn.quilljs.com/1.3.6/quill.js"
      ],
      () => {
        var Size = Quill.import("attributors/class/size");
        Size.whitelist = ["small", "title"];
        Quill.register(Size, true);

        // For some reason the bubble toolbar (that appears when the user selects the text)
        // doesn't work if the element quill attachs to is a custom element. So we need to
        // create a div in order for the toolbar to work correctly
        this.innerHTML = "<div></div>";

        this.quill = new Quill(this.querySelector("div"), {
          theme: "bubble",
          modules: {
            toolbar: [
              ["bold", "italic", "underline"],
              [
                { align: "" },
                { align: "center" },
                { align: "right" },
                { align: "justify" }
              ],
              [{ size: ["small", false, "title"] }],
              [{ list: "ordered" }, { list: "bullet" }, "link"]
            ]
          }
        });

        if (this.getAttribute("data-delta")) {
          try {
            let delta = JSON.parse(this.getAttribute("data-delta"));
            this.quill.setContents(delta);
          } catch (err) {}
        }

        this.quill.on("text-change", () => {
          this.dispatchEvent(
            new CustomEvent("quill-change", {
              detail: {
                text: this.quill.getText(),
                html: this.querySelector(".ql-editor").innerHTML,
                delta: this.quill.getContents()
              }
            })
          );
        });

        this.quill.focus();
      }
    );
  }

  disconnectedCallback() {
    console.log("disconnected!");
  }
}

window.customElements.define("websync-quill", WebsyncQuill);
