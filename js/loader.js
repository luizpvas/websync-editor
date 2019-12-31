let loadedUrls = [];

export function loadJs(url, callback) {
  if (alreadyLoaded(url)) {
    return callback();
  }

  let script = document.createElement("script");
  script.src = url;
  script.onload = () => {
    loadedUrls.push(url);
    callback();
  };
  document.head.appendChild(script);
}

export function loadCss(url, callback) {
  if (alreadyLoaded(url)) {
    return callback();
  }

  let link = document.createElement("link");
  link.rel = "stylesheet";
  link.href = url;
  link.onload = () => {
    loadedUrls.push(url);
    callback();
  };
  document.head.appendChild(link);
}

export function load(urls, callback) {
  let loadUrl = function() {
    let url = urls.pop();
    if (!url) {
      return callback();
    }

    if (url.indexOf(".js") !== -1) {
      loadJs(url, loadUrl);
    } else if (url.indexOf(".css") !== -1) {
      loadCss(url, loadUrl);
    } else {
      throw new Error("could not understand URL type: " + url);
    }
  };

  loadUrl();
}

function alreadyLoaded(url) {
  return loadedUrls.indexOf(url) !== -1;
}
