"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[9809],{3905:(e,t,r)=>{r.d(t,{Zo:()=>p,kt:()=>k});var n=r(67294);function o(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function l(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){o(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function i(e,t){if(null==e)return{};var r,n,o=function(e,t){if(null==e)return{};var r,n,o={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(o[r]=e[r]);return o}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(o[r]=e[r])}return o}var u=n.createContext({}),c=function(e){var t=n.useContext(u),r=t;return e&&(r="function"==typeof e?e(t):l(l({},t),e)),r},p=function(e){var t=c(e.components);return n.createElement(u.Provider,{value:t},e.children)},s="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var r=e.components,o=e.mdxType,a=e.originalType,u=e.parentName,p=i(e,["components","mdxType","originalType","parentName"]),s=c(r),m=o,k=s["".concat(u,".").concat(m)]||s[m]||d[m]||a;return r?n.createElement(k,l(l({ref:t},p),{},{components:r})):n.createElement(k,l({ref:t},p))}));function k(e,t){var r=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var a=r.length,l=new Array(a);l[0]=m;var i={};for(var u in t)hasOwnProperty.call(t,u)&&(i[u]=t[u]);i.originalType=e,i[s]="string"==typeof e?e:o,l[1]=i;for(var c=2;c<a;c++)l[c]=r[c];return n.createElement.apply(null,l)}return n.createElement.apply(null,r)}m.displayName="MDXCreateElement"},54392:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>u,contentTitle:()=>l,default:()=>d,frontMatter:()=>a,metadata:()=>i,toc:()=>c});var n=r(87462),o=(r(67294),r(3905));const a={sidebar_position:2},l="\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3",i={unversionedId:"layer-two",id:"layer-two",title:"\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3",description:"\u30ec\u30a4\u30e4\u30fc2\u3068\u306f\u3069\u3046\u3044\u3046\u610f\u5473\u3067\u3059\u304b\uff1f\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u306b\u306f\u3069\u306e\u3088\u3046\u306a\u7a2e\u985e\u304c\u3042\u308a\u307e\u3059\u304b\uff1f",source:"@site/i18n/ja/docusaurus-plugin-content-docs-core-concepts/current/layer-two.md",sourceDirName:".",slug:"/layer-two",permalink:"/head-protocol/unstable/ja/core-concepts/layer-two",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/i18n/ja/docusaurus-plugin-content-docs-core-concepts/current/layer-two.md",tags:[],version:"current",sidebarPosition:2,frontMatter:{sidebar_position:2},sidebar:"defaultSidebar",previous:{title:"Hydra Networking",permalink:"/head-protocol/unstable/ja/core-concepts/architecture/networking"},next:{title:"API Behavior",permalink:"/head-protocol/unstable/ja/core-concepts/behavior"}},u={},c=[{value:"\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb",id:"\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb",level:2},{value:"\u4e8b\u4f8b",id:"\u4e8b\u4f8b",level:4},{value:"\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3",id:"\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3",level:2},{value:"\u4e8b\u4f8b",id:"\u4e8b\u4f8b-1",level:4},{value:"\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7",id:"\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7",level:2},{value:"\u4e8b\u4f8b",id:"\u4e8b\u4f8b-2",level:4}],p={toc:c},s="wrapper";function d(e){let{components:t,...r}=e;return(0,o.kt)(s,(0,n.Z)({},p,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3"},"\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3"),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},"\u30ec\u30a4\u30e4\u30fc2\u3068\u306f\u3069\u3046\u3044\u3046\u610f\u5473\u3067\u3059\u304b\uff1f\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u306b\u306f\u3069\u306e\u3088\u3046\u306a\u7a2e\u985e\u304c\u3042\u308a\u307e\u3059\u304b\uff1f")),(0,o.kt)("p",null,"\u30d6\u30ed\u30c3\u30af\u30c1\u30a7\u30fc\u30f3\u696d\u754c\u3067\u306f\u3001\u65e2\u5b58\u306e\u30d7\u30ed\u30c8\u30b3\u30eb\uff08\u30ec\u30a4\u30e4\u30fc1\uff09\u3092\u30aa\u30fc\u30d0\u30fc\u30ec\u30a4\u3057\u3066\u3001\u57fa\u76e4\u3068\u306a\u308b\u30d7\u30ed\u30c8\u30b3\u30eb\u306b\u5bfe\u3057\u3066\u8ffd\u52a0\u6a5f\u80fd\u307e\u305f\u306f\u6027\u80fd\u4e0a\u306e\u5229\u70b9\u3092\u63d0\u4f9b\u3059\u308b\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u3092\u6307\u3059\u5834\u5408\u3001",(0,o.kt)("strong",{parentName:"p"},"\u30ec\u30a4\u30e4\u30fc2"),"\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u3068\u547c\u3093\u3067\u3044\u307e\u3059\u3002\u672c\u8cea\u7684\u306b\u306f\u3001\u305d\u308c\u3089\u306f\u4ed6\u306e\u30a2\u30d7\u30ea\u30b1\u30fc\u30b7\u30e7\u30f3\u3092\u53ef\u80fd\u306b\u3059\u308b\u305f\u3081\u306b\u30d7\u30ed\u30c8\u30b3\u30eb\u306e\u4e0a\u306b\u69cb\u7bc9\u3055\u308c\u305f\u6c4e\u7528\u7684\u306a\u76ee\u7684\u306e\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u3067\u3001\u30ec\u30a4\u30e41\u30d7\u30ed\u30c8\u30b3\u30eb\u3068\u7570\u306a\u308b\u30c8\u30ec\u30fc\u30c9\u30aa\u30d5\u3092\u6301\u3064\u5206\u6563\u578b\u30a2\u30d7\u30ea\u30b1\u30fc\u30b7\u30e7\u30f3\u3092\u958b\u767a\u3059\u308b\u305f\u3081\u306e\u30d5\u30ec\u30fc\u30e0\u30ef\u30fc\u30af\u3092\u63d0\u4f9b\u3057\u307e\u3059\u3002"),(0,o.kt)("h2",{id:"\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb"},"\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb"),(0,o.kt)("p",null,"Hydra Head\u30d7\u30ed\u30c8\u30b3\u30eb\u306f\u30ec\u30a4\u30e4\u30fc2\u306e\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u3067\u3001",(0,o.kt)("strong",{parentName:"p"},"\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb"),"\u306b\u5c5e\u3057\u3001\u305d\u308c\u81ea\u4f53\u304c",(0,o.kt)("strong",{parentName:"p"},"\u30da\u30a4\u30e1\u30f3\u30c8\u30c1\u30e3\u30cd\u30eb"),"\u306e\u5b50\u5b6b\u306b\u306a\u308a\u307e\u3059\u3002\u30da\u30a4\u30e1\u30f3\u30c8\u30c1\u30e3\u30cd\u30eb\u306f\u30012\u3064\u4ee5\u4e0a\u306e\u5f53\u4e8b\u8005\u304c\u3001\u57fa\u790e\u3068\u306a\u308b\u30d6\u30ed\u30c3\u30af\u30c1\u30a7\u30fc\u30f3\u306b\u3059\u3079\u3066\u306e\u30c8\u30e9\u30f3\u30b6\u30af\u30b7\u30e7\u30f3\u3092\u30b3\u30df\u30c3\u30c8\u3059\u308b\u3053\u3068\u306a\u304f\u3001\u7279\u5b9a\u306e\u30aa\u30d5\u30c1\u30a7\u30fc\u30f3\u30d7\u30ed\u30c8\u30b3\u30eb\u306b\u5f93\u3063\u3066\u8cc7\u91d1\u3092\u4ea4\u63db\u3059\u308b\u3053\u3068\u3092\u53ef\u80fd\u306b\u3057\u307e\u3059\u3002\u3053\u308c\u306f\u6b74\u53f2\u7684\u306b\u3001\u30d1\u30fc\u30df\u30c3\u30b7\u30e7\u30f3\u30ec\u30b9\u53f0\u5e33\u306e\u30b9\u30b1\u30fc\u30e9\u30d3\u30ea\u30c6\u30a3\u306e\u554f\u984c\u306b\u5bfe\u3059\u308b\u7b54\u3048\u3068\u3057\u3066\u751f\u307e\u308c\u305f\u6700\u521d\u306e\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u306e\u4e00\u7a2e\u3067\u3059\uff08\u305d\u306e\u7d50\u679c\u3001\u6700\u3082\u7814\u7a76\u3055\u308c\u3066\u3044\u308b\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u306e\u4e00\u7a2e\u3067\u3082\u3042\u308a\u307e\u3059\uff09\u3002"),(0,o.kt)("p",null,"\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb\u306f\u3001\u5f93\u6765\u306e\u6c7a\u6e08\u30c1\u30e3\u30cd\u30eb\u306e\u6982\u5ff5\u3092\u62e1\u5f35\u3057\u3001\u30aa\u30d5\u30c1\u30a7\u30fc\u30f3\u30c1\u30e3\u30cd\u30eb\u4e0a\u3067\u30b9\u30de\u30fc\u30c8\u30b3\u30f3\u30c8\u30e9\u30af\u30c8\u3092\u30b5\u30dd\u30fc\u30c8\u3057\u307e\u3059\u3002\u3053\u306e\u3088\u3046\u306a\u30bb\u30c3\u30c8\u30a2\u30c3\u30d7\u3067\u306f\u30011\u3064\u307e\u305f\u306f\u8907\u6570\u306e\u30d1\u30fc\u30c6\u30a3\u306f\u3082\u306f\u3084\u7d14\u7c8b\u306a\u53d6\u5f15\u652f\u6255\u3044\u306b\u5236\u9650\u3055\u308c\u308b\u3053\u3068\u306a\u304f\u3001\u30aa\u30d5\u30c1\u30a7\u30fc\u30f3\u3067\u8907\u96d1\u306a\u30ed\u30b8\u30c3\u30af\u3092\u6271\u3046\u672c\u683c\u7684\u306a\u30b9\u30af\u30ea\u30d7\u30c8\u691c\u8a3c\u3092\u5b9f\u884c\u3057\u3001\u5f8c\u3067\u7d50\u679c\u3092\u30ec\u30a4\u30e4\u30fc1\u306b\u30b3\u30df\u30c3\u30c8\u3059\u308b\u3060\u3051\u3067\u3088\u3044\u306e\u3067\u3059\u3002"),(0,o.kt)("h4",{id:"\u4e8b\u4f8b"},"\u4e8b\u4f8b"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Lightning (Bitcoin);"),(0,o.kt)("li",{parentName:"ul"},"Perun (Ethereum, Polkadot, Cosmos);"),(0,o.kt)("li",{parentName:"ul"},"Sprites (Ethereum);"),(0,o.kt)("li",{parentName:"ul"},"\u79c1\u9054\u306e\u304a\u6c17\u306b\u5165\u308a\u3067\u3042\u308b: ",(0,o.kt)("strong",{parentName:"li"},"Hydra: Head")," (Cardano)!")),(0,o.kt)("h2",{id:"\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3"},"\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3"),(0,o.kt)("p",null,(0,o.kt)("strong",{parentName:"p"},"\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3")," \u3092\u4f7f\u7528\u3059\u308b\u3068\u3001\u72ec\u81ea\u306e\u30b3\u30f3\u30bb\u30f3\u30b5\u30b9\u30eb\u30fc\u30eb\u306e\u30bb\u30c3\u30c8\u3092\u4f7f\u7528\u3057\u3066\u3001\u30a2\u30bb\u30c3\u30c8\u3092\u30ec\u30a4\u30e4\u30fc1\u30d7\u30ed\u30c8\u30b3\u30eb\u304b\u3089\u65b0\u3057\u3044\u30c1\u30a7\u30fc\u30f3\u306b\u8ee2\u9001\u3067\u304d\u307e\u3059\u3002\u901a\u5e38\u3001\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3\u306f\u3001\u3088\u308a\u5358\u7d14\u3067\u3088\u308a\u52b9\u7387\u7684\u306a\u30b3\u30f3\u30bb\u30f3\u30b5\u30b9\u30e1\u30ab\u30cb\u30ba\u30e0\u3092\u63d0\u4f9b\u3057\u307e\u3059\u3002\u3053\u308c\u306b\u3088\u308a\u3001\u30b9\u30b1\u30fc\u30e9\u30d3\u30ea\u30c6\u30a3\u304c\u5411\u4e0a\u3059\u308b\u304b\u3001\u30ec\u30a4\u30e4\u30fc1\u3067\u63a1\u7528\u3059\u308b\u306e\u304c\u96e3\u3057\u3044\u65b0\u6a5f\u80fd\u306e\u5b9f\u88c5\u304c\u5bb9\u6613\u306b\u306a\u308a\u307e\u3059\u3002\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3\u306f\u901a\u5e38\u3001\u30eb\u30fc\u30c8\u306b\u5c11\u6570\u306e\u30a2\u30af\u30bf\u30fc\u307e\u305f\u306f\u59d4\u54e1\u4f1a\u306e\u307f\u304c\u95a2\u4e0e\u3057\u3066\u3044\u307e\u3059\u3002"),(0,o.kt)("p",null,"\u305f\u3060\u3057\u3001\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3\u306f\u300c\u9069\u5207\u306a\u30c1\u30a7\u30fc\u30f3\u300d\u3067\u3042\u308a\u3001\u30d6\u30ed\u30c3\u30af\u306f\u30d0\u30ea\u30c7\u30fc\u30bf\u30fc\u306b\u3088\u3063\u3066\u751f\u6210\u3055\u308c\u3001\u901a\u5e38\u306f\u30b9\u30de\u30fc\u30c8\u30b3\u30f3\u30c8\u30e9\u30af\u30c8\u6a5f\u80fd\u3092\u5099\u3048\u3066\u3044\u307e\u3059\u3002\u3057\u305f\u304c\u3063\u3066\u3001\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb\u3068\u306f\u7570\u306a\u308a\u3001\u30c7\u30fc\u30bf\u306e\u53ef\u7528\u6027\u3068\u3001\u30c1\u30a7\u30fc\u30f3\u306e\u691c\u8a3c\u3068\u89b3\u5bdf\u306b\u53c2\u52a0\u3059\u308b\u65b9\u6cd5\u3092\u63d0\u4f9b\u3057\u307e\u3059\uff08\u30b9\u30c6\u30fc\u30c8\u30c1\u30e3\u30cd\u30eb\u3067\u306f\u3001\u30c1\u30e3\u30cd\u30eb\u306e\u53c2\u52a0\u8005\u3060\u3051\u304c\u3001\u30c1\u30e3\u30cd\u30eb\u3067\u4f55\u304c\u8d77\u3053\u3063\u3066\u3044\u308b\u304b\u306b\u3064\u3044\u3066\u4fe1\u983c\u3067\u304d\u308b\u30d3\u30e5\u30fc\u3092\u6301\u3063\u3066\u3044\u307e\u3059\u3002 \uff09\u3002\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3\u306b\u5165\u308b\u306b\u306f\u3001\u901a\u5e38\u3001\u30ec\u30a4\u30e4\u30fc1\u3067\u30a2\u30bb\u30c3\u30c8\u3092\u713c\u304d\u4ed8\u3051\u308b\u304b\u30ed\u30c3\u30af\u3057\u3066\u3001\u30b5\u30a4\u30c9\u30c1\u30a7\u30fc\u30f3\u30cd\u30c3\u30c8\u30ef\u30fc\u30af\u3067\u540c\u7b49\u306e\u30a2\u30bb\u30c3\u30c8\u3092\u53d7\u3051\u53d6\u308a\u307e\u3059\u3002"),(0,o.kt)("h4",{id:"\u4e8b\u4f8b-1"},"\u4e8b\u4f8b"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Liquid Network (Bitcoin);"),(0,o.kt)("li",{parentName:"ul"},"RSK (Bitcoin);"),(0,o.kt)("li",{parentName:"ul"},"Polygon (Ethereum);"),(0,o.kt)("li",{parentName:"ul"},"Milkomeda (Cardano).")),(0,o.kt)("h2",{id:"\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7"},"\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7"),(0,o.kt)("p",null,"\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3\u306e\u3082\u30461\u3064\u306e\u4e3b\u8981\u306a\u30bf\u30a4\u30d7\u306f\u3001\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\u3067\u3059\u3002\u3053\u308c\u3089\u306f\u3001\u30c8\u30e9\u30f3\u30b6\u30af\u30b7\u30e7\u30f3\u306e\u5b9f\u884c\u3092\u30aa\u30d5\u30c1\u30a7\u30fc\u30f3\u306b\u79fb\u52d5\u3057\u3066\u3001\u30ec\u30a4\u30e4\u30fc1\u3067\u306e\u5b9f\u884c\u306e\u8868\u73fe\u3092\u306f\u308b\u304b\u306b\u30b3\u30f3\u30d1\u30af\u30c8\u306b\u4fdd\u3064\u200b\u200b\u65b9\u6cd5\u3092\u63d0\u4f9b\u3057\u307e\u3059\u3002\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\u306f\u901a\u5e38\u3001\u691c\u8a3c\u53ef\u80fd\u306a\u30d6\u30ec\u30c3\u30c9\u30af\u30e9\u30e0\u3092\u5b9a\u671f\u7684\u306b\u6b8b\u3057\u306a\u304c\u3089\u3001\u9ad8\u53ef\u7528\u6027\u3068\u9ad8\u3044\u8a08\u7b97\u6a5f\u80fd\u3092\u30aa\u30d5\u30c1\u30a7\u30fc\u30f3\u3067\u63d0\u4f9b\u3059\u308b\u4e2d\u592e\u30a2\u30af\u30bf\u30fc\u306b\u3088\u3063\u3066\u99c6\u52d5\u3055\u308c\u307e\u3059\u3002\u30aa\u30f3\u30c1\u30a7\u30fc\u30f3\uff08\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\uff09\u3002"),(0,o.kt)("p",null,"\u4e00\u822c\u306b\u3001\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\u306b\u306f\u3001\u697d\u89b3\u7684\u307e\u305f\u306f\u30bc\u30ed\u77e5\u8b58\u306e2\u3064\u306e\u30d5\u30ec\u30fc\u30d0\u30fc\u304c\u3042\u308a\u307e\u3059\u3002\u524d\u8005\u3067\u306f\u3001\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\u306f\u697d\u89b3\u7684\u306b\u30c1\u30a7\u30fc\u30f3\u306b\u6295\u7a3f\u3055\u308c\u3001\u691c\u8a3c\u306f\u72ec\u7acb\u3057\u305f\u30d0\u30ea\u30c7\u30fc\u30bf\u30fc\u306b\u3088\u3063\u3066\u4e8b\u5f8c\u7684\u306b\u884c\u308f\u308c\u307e\u3059\u3002\u610f\u898b\u306e\u76f8\u9055\u304c\u3042\u308b\u5834\u5408\u3001\u7d1b\u4e89\u306f\u30c1\u30a7\u30fc\u30f3\u4e0a\u3067\u89e3\u6c7a\u3055\u308c\u3001\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\u767a\u884c\u8005\u306f\u7d4c\u6e08\u7684\u5f71\u97ff\u306b\u8010\u3048\u307e\u3059\u3002\u30bc\u30ed\u77e5\u8b58\u30a2\u30d7\u30ed\u30fc\u30c1\u3067\u306f\u3001\u5b9f\u884c\u306e\u7c21\u6f54\u306a\u8a3c\u660e\u304c\u30aa\u30d5\u30c1\u30a7\u30fc\u30f3\u3067\u8a08\u7b97\u3055\u308c\u3001\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\u3068\u4e00\u7dd2\u306b\u516c\u958b\u3055\u308c\u3001\u30aa\u30f3\u30c1\u30a7\u30fc\u30f3\u30d0\u30ea\u30c7\u30fc\u30bf\u30fc\u306b\u3088\u3063\u3066\u5236\u5fa1\u3055\u308c\u307e\u3059\uff08\u3057\u305f\u304c\u3063\u3066\u3001\u30ed\u30fc\u30eb\u30a2\u30c3\u30d7\u306e\u6b63\u5f53\u306a\u5b9f\u884c\u304c\u5f37\u5236\u3055\u308c\u307e\u3059\uff09"),(0,o.kt)("h4",{id:"\u4e8b\u4f8b-2"},"\u4e8b\u4f8b"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Arbitrum (Ethereum);"),(0,o.kt)("li",{parentName:"ul"},"Optimism (Ethereum);"),(0,o.kt)("li",{parentName:"ul"},"Hermez (Ethereum);"),(0,o.kt)("li",{parentName:"ul"},"ZKSync (Ethereum).")))}d.isMDXComponent=!0}}]);