import { timingSafeEqual } from "crypto";

export default class CtrlZ {
  constructor(editor) {
    this.editor = editor;
    this.trixValues = [];

    this.handler = document.addEventListener("keydown", ev => {
      if (ev.keyCode == 90 && ev.ctrlKey) {
        if (this.hasTrix()) {
          this.trackTrixContent();

          if (this.trixValues.length >= 2) {
            if (
              this.trixValues[this.trixValues.length - 1] ==
              this.trixValues[this.trixValues.length - 2]
            ) {
              this.callback();
            }
          }
        } else {
          this.trixValues = [];
          this.callback();
        }
      }
    });
  }

  hasTrix() {
    return this.editor.querySelector("trix-editor");
  }

  trackTrixContent() {
    this.trixValues.push(this.currentTrixContent());
  }

  currentTrixContent() {
    return this.editor.querySelector("websync-trix input").value;
  }

  listen(callback) {
    this.callback = callback;
  }

  stop() {
    document.removeEventListener("keydown", this.handler);
  }
}
