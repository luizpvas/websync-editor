import MicroModal from "micromodal";

export default function open(opts) {
  let modal = `
    <div id="modal-1" class="modal" aria-hidden="true">
      <div tabindex="-1" class="backdrop" data-micromodal-close>
        <div class="main" role="dialog" aria-modal="true" aria-labelledby="modal-1-title" style="width: ${opts.width};">
          ${opts.html}
        </div>
      </div>
    </div>
  `;

  document.body.insertAdjacentHTML("beforeend", modal);
  MicroModal.show("modal-1", {
    onClose: modal => {
      modal.parentNode.removeChild(modal);
    }
  });
}
