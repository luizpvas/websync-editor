class WebsyncHtml extends HTMLElement {
  connectedCallback() {
    this.innerHTML = this.getAttribute("data-html");

    let observer = new MutationObserver(mutations => {
      this.innerHTML = this.getAttribute("data-html");
    });

    observer.observe(this, {
      attributes: true
    });
  }
}

window.customElements.define("websync-html", WebsyncHtml);
