import "../css/main.scss";
import "./websync-quill";
import "./websync-html";
import "./websync-file-upload";
import { Elm } from "../elm/Main.elm";

class WebsyncEditor extends HTMLElement {
  connectedCallback() {
    this.innerHTML = "<div></div>";

    this.app = Elm.Main.init({
      node: this.querySelector("div"),
      flags: {
        latestId: new Date().getTime()
      }
    });
  }

  uploadHandler(callback) {
    this.uploadHandlerCallback = callback;
  }

  fileSelected(file, contentId) {
    let progress = percentage => {
      this.app.ports.uploadProgress.send({
        percentage: parseInt(percentage),
        contentId: parseInt(contentId)
      });
    };

    let done = url => {
      this.app.ports.uploadDone.send({ url, contentId: parseInt(contentId) });
    };

    this.uploadHandlerCallback(file, progress, done);
  }

  disconnectedCallback() {
    console.log("disconnected!");
  }
}

window.customElements.define("websync-editor", WebsyncEditor);
