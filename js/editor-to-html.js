let themeDuo = require("css-to-string-loader!css-loader!../css/themes/duo.css");

export default function editorToHtml(editor) {
  let email = editor.cloneNode(true);

  Array.from(email.querySelectorAll(".ws-selected")).forEach(node => {
    node.parentNode.removeChild(node);
  });

  Array.from(email.querySelectorAll("websync-html")).forEach(node => {
    node.replaceWith(...node.childNodes);
  });

  Array.from(email.querySelectorAll("[data-url]")).forEach(node => {
    if (node.getAttribute("data-url")) {
      let link = document.createElement("a");
      link.href = node.getAttribute("data-url");
      node.replaceWith(link);
      link.appendChild(node);
    }
  });

  return `
    <html>
      <head>
        <style>
          ${themeDuo}
        </style>
      </head>
      <body style="background: #dfdfdf;">
        ${email.outerHTML}
      </body>
    </html>
  `;
}
