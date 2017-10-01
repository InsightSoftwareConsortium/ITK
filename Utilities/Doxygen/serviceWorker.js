importScripts('workbox-sw.prod.v2.0.1.js');

const workboxSW = new self.WorkboxSW({clientsClaim: true})
workboxSW.precache([
  {
    "url": "doxygen.css",
    "revision": "6443da25058e7338a96c3ce7ee4918bb"
  },
  {
    "url": "dynsections.js",
    "revision": "bcd222771d969778fb39b1e86d54ef8d"
  },
  {
    "url": "ITKDoxygenStyle.css",
    "revision": "5af396c309fa4d2f85607eb13af940f6"
  },
  {
    "url": "itkLogo.jpg",
    "revision": "6ff2ac3eec3a0c283fa068821ff192ff"
  },
  {
    "url": "jquery.js",
    "revision": "b83638ec86d9102531dcc276ca14dd43"
  },
  {
    "url": "search/search.css",
    "revision": "f55d2d0c0650c5a8b71b3270a8436691"
  },
  {
    "url": "search/search.js",
    "revision": "ecd9475c2a7f79a1874ff3784f8214e4"
  },
  {
    "url": "tabs.css",
    "revision": "cf9a4ad5a8b52323af8cc15e96fb2e93"
  },
  {
    "url": "workbox-sw.prod.v2.0.1.js",
    "revision": "679d4e73dc756b21e46ee8f1bb52c882"
  },
  {
    "url": "itkLogoSmall.gif",
    "revision": "33b01611d2d48741c3d82ccc901bdbd3"
  }
])

workboxSW.router.registerRoute(
  /\.html$|\.gif$|\.png$/,
  workboxSW.strategies.cacheFirst({
  cacheName: 'cacheFirstContent',
  cacheExpiration: {
    maxEntries: 100,
    maxAgeSeconds: 7 * 24 * 60 * 60 * 4,
    }
  })
);
