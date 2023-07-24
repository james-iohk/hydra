"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[4753],{3905:(e,t,r)=>{r.d(t,{Zo:()=>p,kt:()=>m});var n=r(67294);function s(e,t,r){return t in e?Object.defineProperty(e,t,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[t]=r,e}function a(e,t){var r=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),r.push.apply(r,n)}return r}function o(e){for(var t=1;t<arguments.length;t++){var r=null!=arguments[t]?arguments[t]:{};t%2?a(Object(r),!0).forEach((function(t){s(e,t,r[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(r)):a(Object(r)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(r,t))}))}return e}function l(e,t){if(null==e)return{};var r,n,s=function(e,t){if(null==e)return{};var r,n,s={},a=Object.keys(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||(s[r]=e[r]);return s}(e,t);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);for(n=0;n<a.length;n++)r=a[n],t.indexOf(r)>=0||Object.prototype.propertyIsEnumerable.call(e,r)&&(s[r]=e[r])}return s}var i=n.createContext({}),u=function(e){var t=n.useContext(i),r=t;return e&&(r="function"==typeof e?e(t):o(o({},t),e)),r},p=function(e){var t=u(e.components);return n.createElement(i.Provider,{value:t},e.children)},c="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},h=n.forwardRef((function(e,t){var r=e.components,s=e.mdxType,a=e.originalType,i=e.parentName,p=l(e,["components","mdxType","originalType","parentName"]),c=u(r),h=s,m=c["".concat(i,".").concat(h)]||c[h]||d[h]||a;return r?n.createElement(m,o(o({ref:t},p),{},{components:r})):n.createElement(m,o({ref:t},p))}));function m(e,t){var r=arguments,s=t&&t.mdxType;if("string"==typeof e||s){var a=r.length,o=new Array(a);o[0]=h;var l={};for(var i in t)hasOwnProperty.call(t,i)&&(l[i]=t[i]);l.originalType=e,l[c]="string"==typeof e?e:s,o[1]=l;for(var u=2;u<a;u++)o[u]=r[u];return n.createElement.apply(null,o)}return n.createElement.apply(null,r)}h.displayName="MDXCreateElement"},13251:(e,t,r)=>{r.r(t),r.d(t,{assets:()=>i,contentTitle:()=>o,default:()=>d,frontMatter:()=>a,metadata:()=>l,toc:()=>u});var n=r(87462),s=(r(67294),r(3905));const a={},o="Test Results for hydra-tui",l={unversionedId:"tests/hydra-tui/hspec-results",id:"tests/hydra-tui/hspec-results",title:"Test Results for hydra-tui",description:"Hydra.TUI.Options",source:"@site/benchmarks/tests/hydra-tui/hspec-results.md",sourceDirName:"tests/hydra-tui",slug:"/tests/hydra-tui/hspec-results",permalink:"/head-protocol/unstable/es/benchmarks/tests/hydra-tui/hspec-results",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/benchmarks/tests/hydra-tui/hspec-results.md",tags:[],version:"current",frontMatter:{},sidebar:"defaultSidebar",previous:{title:"Test Results for hydra-plutus",permalink:"/head-protocol/unstable/es/benchmarks/tests/hydra-plutus/hspec-results"},next:{title:"Test Results for plutus-cbor",permalink:"/head-protocol/unstable/es/benchmarks/tests/plutus-cbor/hspec-results"}},i={},u=[{value:"Hydra.TUI.Options",id:"hydratuioptions",level:2},{value:"Hydra.TUI",id:"hydratui",level:2},{value:"text rendering errors",id:"text-rendering-errors",level:3},{value:"text rendering tests",id:"text-rendering-tests",level:3},{value:"end-to-end smoke tests",id:"end-to-end-smoke-tests",level:3}],p={toc:u},c="wrapper";function d(e){let{components:t,...r}=e;return(0,s.kt)(c,(0,n.Z)({},p,r,{components:t,mdxType:"MDXLayout"}),(0,s.kt)("h1",{id:"test-results-for-hydra-tui"},"Test Results for hydra-tui"),(0,s.kt)("h2",{id:"hydratuioptions"},"Hydra.TUI.Options"),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},"no arguments yield default options"),(0,s.kt)("li",{parentName:"ul"},"parses --connect option"),(0,s.kt)("li",{parentName:"ul"},"parses --testnet-magic option"),(0,s.kt)("li",{parentName:"ul"},"parses --cardano-signing-key option"),(0,s.kt)("li",{parentName:"ul"},"parses --node-socket option"),(0,s.kt)("li",{parentName:"ul"},"parses --version option")),(0,s.kt)("h2",{id:"hydratui"},"Hydra.TUI"),(0,s.kt)("h3",{id:"text-rendering-errors"},"text rendering errors"),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},"should show not enough fuel message and suggestion")),(0,s.kt)("h3",{id:"text-rendering-tests"},"text rendering tests"),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},"should format time with whole values for every unit, not total values")),(0,s.kt)("h3",{id:"end-to-end-smoke-tests"},"end-to-end smoke tests"),(0,s.kt)("ul",null,(0,s.kt)("li",{parentName:"ul"},"starts & renders"),(0,s.kt)("li",{parentName:"ul"},"display feedback long enough"),(0,s.kt)("li",{parentName:"ul"},"supports the init & abort Head life cycle"),(0,s.kt)("li",{parentName:"ul"},"supports the full Head life cycle"),(0,s.kt)("li",{parentName:"ul"},"doesn't allow multiple initializations")))}d.isMDXComponent=!0}}]);