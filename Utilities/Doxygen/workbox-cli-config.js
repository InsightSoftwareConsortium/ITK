module.exports = {
  "globDirectory": "html/",
  "globPatterns": [
    "**/*.{css,js,jpg}",
    "itkLogoSmall.gif"
  ],
  "swSrc": "serviceWorker.js.in",
  "swDest": "html/serviceWorker.js",
  "globIgnores": [
    "../workbox-cli-config.js",
    "serviceWorker.js"
  ]
};
