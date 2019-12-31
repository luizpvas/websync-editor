class WebsyncFileUpload extends HTMLElement {
  connectedCallback() {
    this.innerHTML = `<input type="file">`;

    let input = this.querySelector("input");

    input.addEventListener("change", ev => {
      let editor = this.editor();
      editor.fileSelected(input.files[0], this.getAttribute("data-content-id"));
    });
  }

  editor() {
    let parent = this.parentNode;
    while (parent) {
      if (parent.tagName == "WEBSYNC-EDITOR") {
        return parent;
      }

      parent = parent.parentNode;
    }
    return null;
  }
}

window.customElements.define("websync-file-upload", WebsyncFileUpload);
