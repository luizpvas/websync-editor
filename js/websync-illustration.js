class WebsyncIllustration extends HTMLElement {
  connectedCallback() {
    let url = this.getAttribute("data-url");
    this.latestColor = this.getAttribute("data-color");

    fetch(url)
      .then(res => res.text())
      .then(svg => {
        this.innerHTML = svg;
        this.querySelector("svg").style.width = "50%";
        this.querySelector("svg").style.height = "50%";
      });

    this.addEventListener("click", ev => {
      let event = new CustomEvent("illustration-click", {
        detail: { base64: "data:image/svg+xml;base64," + btoa(this.innerHTML) }
      });

      this.dispatchEvent(event);
    });

    let observer = new MutationObserver(mutations => {
      this.changeColor(this.getAttribute("data-color"));
    });

    observer.observe(this, {
      attributes: true
    });
  }

  changeColor(newColor) {
    let nodes = this.querySelectorAll('[fill="' + this.latestColor + '"]');

    Array.from(nodes).forEach(node => {
      node.setAttribute("fill", newColor);
    });

    this.latestColor = newColor;
  }
}

window.customElements.define("websync-illustration", WebsyncIllustration);
