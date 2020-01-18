import "../css/main.scss";
import "./websync-quill";
import "./websync-html";
import "./websync-file-upload";
import "./websync-dropdown";
import "./websync-trix";
import "./websync-illustration";
import CtrlZ from "./ctrl-z";
import editorToHtml from "./editor-to-html";
import modal from "./modal";
import { Elm } from "../elm/Main.elm";

class WebsyncEditor extends HTMLElement {
  connectedCallback() {
    this.innerHTML = "<div></div>";

    let serialized = null;
    try {
      serialized = JSON.parse(this.getAttribute("data-serialized"));
    } catch (err) {}

    this.app = Elm.Main.init({
      node: this.querySelector("div"),
      flags: {
        latestId: new Date().getTime(),
        mouseOffsetX: parseInt(this.getAttribute("data-mouse-offset-x") || "0"),
        mouseOffsetY: parseInt(this.getAttribute("data-mouse-offset-y") || "0"),
        serialized: serialized
      }
    });

    this.app.ports.sendContents.subscribe(serialized => {
      this.latestSerializedContent = serialized;
    });

    this.app.ports.save.subscribe(serialized => {
      if (this.saveHandlerCallback) {
        this.getContents(serialized => {
          this.saveHandlerCallback(serialized);
        });
      }
    });

    this.app.ports.openPreview.subscribe(width => {
      this.getContents(content => {
        let escapedHtml = content.html
          .replace(/</g, "&lt;")
          .replace(/>/g, "&gt;")
          .replace(/"/g, "&quot;");

        modal({
          width,
          html: `<iframe srcdoc="${escapedHtml}" style="width: 100%; border: none;"></iframe>`
        });
      });
    });

    new CtrlZ(this).listen(() => {
      this.app.ports.ctrlZ.send(null);
    });
  }

  load(json) {
    this.app.ports.load.send(json);
  }

  uploadHandler(callback) {
    this.uploadHandlerCallback = callback;
  }

  saveHandler(callback) {
    this.saveHandlerCallback = callback;
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

  setIllustrations(illustrations) {
    this.app.ports.illustrations.send(illustrations);
  }

  getContents(callback) {
    this.app.ports.getContents.send(null);
    setTimeout(() => {
      callback({
        json: this.latestSerializedContent,
        html: editorToHtml(this.querySelector(".ws-email"))
      });
    }, 100);
  }
}

window.customElements.define("websync-editor", WebsyncEditor);
