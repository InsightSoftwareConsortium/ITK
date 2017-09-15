module.exports = {
  "globDirectory": "html/",
  "globPatterns": [
    "**/*.{css,js,jpg,php}",
    "itkLogoSmall.gif"
  ],
  "swSrc": "serviceWorker.js.in",
  "swDest": "html/serviceWorker.js",
  "globIgnores": [
    "../workbox-cli-config.js",
    "serviceWorker.js"
  ]
};
