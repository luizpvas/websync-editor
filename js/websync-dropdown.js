class WebsyncDropdown extends HTMLElement {
  connectedCallback() {
    this.style.position = "relative";

    this.trigger().addEventListener("click", () => {
      this.toggle();
    });
  }

  trigger() {
    return this.querySelector("[data-trigger]");
  }

  dropdown() {
    return this.querySelector("[data-dropdown]");
  }

  toggle() {
    if (this.dropdown().style.display == "block") {
      this.close();
    } else {
      this.open();
    }
  }

  open() {
    this.dropdown().style.display = "block";

    this.documentHandler = ev => {
      if (!this.isDescendant(ev.target)) {
        this.close();
      }
    };
    document.addEventListener("click", this.documentHandler);
  }

  clearDocumentHandler() {
    document.removeEventListener("click", this.documentHandler);
    this.documentHandler = null;
  }

  close() {
    this.clearDocumentHandler();
    this.dropdown().style.display = "";
  }

  isDescendant(target) {
    let parent = target.parentNode;

    while (parent) {
      if (parent == this) {
        return true;
      }

      parent = parent.parentNode;
    }

    return false;
  }

  disconnectedCallback() {
    this.clearDocumentHandler();
  }
}

window.customElements.define("websync-dropdown", WebsyncDropdown);
