"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[2627],{3905:(t,a,e)=>{e.d(a,{Zo:()=>d,kt:()=>h});var r=e(67294);function n(t,a,e){return a in t?Object.defineProperty(t,a,{value:e,enumerable:!0,configurable:!0,writable:!0}):t[a]=e,t}function i(t,a){var e=Object.keys(t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(t);a&&(r=r.filter((function(a){return Object.getOwnPropertyDescriptor(t,a).enumerable}))),e.push.apply(e,r)}return e}function l(t){for(var a=1;a<arguments.length;a++){var e=null!=arguments[a]?arguments[a]:{};a%2?i(Object(e),!0).forEach((function(a){n(t,a,e[a])})):Object.getOwnPropertyDescriptors?Object.defineProperties(t,Object.getOwnPropertyDescriptors(e)):i(Object(e)).forEach((function(a){Object.defineProperty(t,a,Object.getOwnPropertyDescriptor(e,a))}))}return t}function g(t,a){if(null==t)return{};var e,r,n=function(t,a){if(null==t)return{};var e,r,n={},i=Object.keys(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||(n[e]=t[e]);return n}(t,a);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(t);for(r=0;r<i.length;r++)e=i[r],a.indexOf(e)>=0||Object.prototype.propertyIsEnumerable.call(t,e)&&(n[e]=t[e])}return n}var m=r.createContext({}),p=function(t){var a=r.useContext(m),e=a;return t&&(e="function"==typeof t?t(a):l(l({},a),t)),e},d=function(t){var a=p(t.components);return r.createElement(m.Provider,{value:a},t.children)},k="mdxType",N={inlineCode:"code",wrapper:function(t){var a=t.children;return r.createElement(r.Fragment,{},a)}},o=r.forwardRef((function(t,a){var e=t.components,n=t.mdxType,i=t.originalType,m=t.parentName,d=g(t,["components","mdxType","originalType","parentName"]),k=p(e),o=n,h=k["".concat(m,".").concat(o)]||k[o]||N[o]||i;return e?r.createElement(h,l(l({ref:a},d),{},{components:e})):r.createElement(h,l({ref:a},d))}));function h(t,a){var e=arguments,n=a&&a.mdxType;if("string"==typeof t||n){var i=e.length,l=new Array(i);l[0]=o;var g={};for(var m in a)hasOwnProperty.call(a,m)&&(g[m]=a[m]);g.originalType=t,g[k]="string"==typeof t?t:n,l[1]=g;for(var p=2;p<i;p++)l[p]=e[p];return r.createElement.apply(null,l)}return r.createElement.apply(null,e)}o.displayName="MDXCreateElement"},97642:(t,a,e)=>{e.r(a),e.d(a,{assets:()=>m,contentTitle:()=>l,default:()=>N,frontMatter:()=>i,metadata:()=>g,toc:()=>p});var r=e(87462),n=(e(67294),e(3905));const i={sidebar_label:"Transactions Costs",sidebar_position:3},l="Transactions Costs",g={unversionedId:"transaction-cost",id:"transaction-cost",title:"Transactions Costs",description:"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using arbitrary values and results are not fully deterministic and comparable to previous runs.",source:"@site/benchmarks/transaction-cost.md",sourceDirName:".",slug:"/transaction-cost",permalink:"/head-protocol/unstable/es/benchmarks/transaction-cost",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/benchmarks/transaction-cost.md",tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_label:"Transactions Costs",sidebar_position:3},sidebar:"defaultSidebar",previous:{title:"Benchmarks",permalink:"/head-protocol/unstable/es/benchmarks/"},next:{title:"End-to-End Benchmarks",permalink:"/head-protocol/unstable/es/benchmarks/end-to-end-benchmarks"}},m={},p=[{value:"Script summary",id:"script-summary",level:2},{value:"Cost of Init Transaction",id:"cost-of-init-transaction",level:2},{value:"Cost of Commit Transaction",id:"cost-of-commit-transaction",level:2},{value:"Cost of CollectCom Transaction",id:"cost-of-collectcom-transaction",level:2},{value:"Cost of Close Transaction",id:"cost-of-close-transaction",level:2},{value:"Cost of Contest Transaction",id:"cost-of-contest-transaction",level:2},{value:"Cost of Abort Transaction",id:"cost-of-abort-transaction",level:2},{value:"Cost of FanOut Transaction",id:"cost-of-fanout-transaction",level:2}],d={toc:p},k="wrapper";function N(t){let{components:a,...e}=t;return(0,n.kt)(k,(0,r.Z)({},d,e,{components:a,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"transactions-costs"},"Transactions Costs"),(0,n.kt)("p",null,"Sizes and execution budgets for Hydra protocol transactions. Note that unlisted parameters are currently using ",(0,n.kt)("inlineCode",{parentName:"p"},"arbitrary")," values and results are not fully deterministic and comparable to previous runs."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Metadata"),(0,n.kt)("th",{parentName:"tr",align:"left"}))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Generated at")),(0,n.kt)("td",{parentName:"tr",align:"left"},"2023-07-24 04:15:03.877457176 UTC")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. memory units")),(0,n.kt)("td",{parentName:"tr",align:"left"},"14000000")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. CPU units")),(0,n.kt)("td",{parentName:"tr",align:"left"},"10000000000")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},(0,n.kt)("em",{parentName:"td"},"Max. tx size (kB)")),(0,n.kt)("td",{parentName:"tr",align:"left"},"16384")))),(0,n.kt)("h2",{id:"script-summary"},"Script summary"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Name"),(0,n.kt)("th",{parentName:"tr",align:"left"},"Hash"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Size (Bytes)"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdInitial"),(0,n.kt)("td",{parentName:"tr",align:"left"},"7ceb53f05e444cfdabfd0a37a0590090066da457a1f1db30d613b8bd"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4289")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdCommit"),(0,n.kt)("td",{parentName:"tr",align:"left"},"70e70fc13217bfde96932956656c1d540a743b1588c845ca09dc3723"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2124")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bdHead"),(0,n.kt)("td",{parentName:"tr",align:"left"},"cda51d313c1c8285b6925ce2413def012db27f544e2bbd79b8173000"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9185")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"\u03bcHead"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1c0b665fc49bc2e9e2ce4e8252c8f37fe84dd75bd8e086abfdb92685*"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4149")))),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"The minting policy hash is only usable for comparison. As the script is parameterized, the actual script is unique per Head.")),(0,n.kt)("h2",{id:"cost-of-init-transaction"},"Cost of Init Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4747"),(0,n.kt)("td",{parentName:"tr",align:"right"},"14.48"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.52")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4947"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.81"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.14"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.54")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5152"),(0,n.kt)("td",{parentName:"tr",align:"right"},"17.34"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6.69"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.57")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5563"),(0,n.kt)("td",{parentName:"tr",align:"right"},"22.08"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.48"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.63")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6587"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.13"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13.82"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.83")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"37"),(0,n.kt)("td",{parentName:"tr",align:"right"},"12124"),(0,n.kt)("td",{parentName:"tr",align:"right"},"97.38"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.81"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.74")))),(0,n.kt)("h2",{id:"cost-of-commit-transaction"},"Cost of Commit Transaction"),(0,n.kt)("p",null," This is using ada-only outputs for better comparability."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"599"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.06"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5.76"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.34")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"783"),(0,n.kt)("td",{parentName:"tr",align:"right"},"19.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.73"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.40")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"966"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.85"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.91"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.47")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1348"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.19"),(0,n.kt)("td",{parentName:"tr",align:"right"},"14.60"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.61")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2283"),(0,n.kt)("td",{parentName:"tr",align:"right"},"72.00"),(0,n.kt)("td",{parentName:"tr",align:"right"},"28.94"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.04")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"13"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2842"),(0,n.kt)("td",{parentName:"tr",align:"right"},"98.24"),(0,n.kt)("td",{parentName:"tr",align:"right"},"39.22"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.35")))),(0,n.kt)("h2",{id:"cost-of-collectcom-transaction"},"Cost of CollectCom Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO (bytes)"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"823"),(0,n.kt)("td",{parentName:"tr",align:"right"},"27.74"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.77"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.49")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"left"},"114"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1136"),(0,n.kt)("td",{parentName:"tr",align:"right"},"43.86"),(0,n.kt)("td",{parentName:"tr",align:"right"},"17.14"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.68")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"left"},"171"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1454"),(0,n.kt)("td",{parentName:"tr",align:"right"},"62.24"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.48"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.89")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"4"),(0,n.kt)("td",{parentName:"tr",align:"left"},"226"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1773"),(0,n.kt)("td",{parentName:"tr",align:"right"},"83.25"),(0,n.kt)("td",{parentName:"tr",align:"right"},"32.89"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.14")))),(0,n.kt)("h2",{id:"cost-of-close-transaction"},"Cost of Close Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"635"),(0,n.kt)("td",{parentName:"tr",align:"right"},"18.98"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.46"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.39")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"804"),(0,n.kt)("td",{parentName:"tr",align:"right"},"20.08"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.60"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.42")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"977"),(0,n.kt)("td",{parentName:"tr",align:"right"},"21.78"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.97"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.45")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1299"),(0,n.kt)("td",{parentName:"tr",align:"right"},"24.57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13.49"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.50")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2120"),(0,n.kt)("td",{parentName:"tr",align:"right"},"31.57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"19.78"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.64")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"50"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8725"),(0,n.kt)("td",{parentName:"tr",align:"right"},"87.55"),(0,n.kt)("td",{parentName:"tr",align:"right"},"70.11"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.75")))),(0,n.kt)("h2",{id:"cost-of-contest-transaction"},"Cost of Contest Transaction"),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"701"),(0,n.kt)("td",{parentName:"tr",align:"right"},"25.44"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.06"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.47")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"841"),(0,n.kt)("td",{parentName:"tr",align:"right"},"26.59"),(0,n.kt)("td",{parentName:"tr",align:"right"},"12.04"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.49")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1005"),(0,n.kt)("td",{parentName:"tr",align:"right"},"28.31"),(0,n.kt)("td",{parentName:"tr",align:"right"},"13.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.52")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1336"),(0,n.kt)("td",{parentName:"tr",align:"right"},"31.32"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.99"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.58")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"right"},"2152"),(0,n.kt)("td",{parentName:"tr",align:"right"},"40.34"),(0,n.kt)("td",{parentName:"tr",align:"right"},"23.01"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.74")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"44"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7772"),(0,n.kt)("td",{parentName:"tr",align:"right"},"98.37"),(0,n.kt)("td",{parentName:"tr",align:"right"},"69.48"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.79")))),(0,n.kt)("h2",{id:"cost-of-abort-transaction"},"Cost of Abort Transaction"),(0,n.kt)("p",null,"Some variation because of random mixture of still initial and already committed outputs."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4857"),(0,n.kt)("td",{parentName:"tr",align:"right"},"22.64"),(0,n.kt)("td",{parentName:"tr",align:"right"},"9.46"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.62")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"2"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5180"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.96"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.65"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.79")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"3"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5354"),(0,n.kt)("td",{parentName:"tr",align:"right"},"49.66"),(0,n.kt)("td",{parentName:"tr",align:"right"},"20.96"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.94")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"4"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5818"),(0,n.kt)("td",{parentName:"tr",align:"right"},"74.34"),(0,n.kt)("td",{parentName:"tr",align:"right"},"31.87"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.24")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5855"),(0,n.kt)("td",{parentName:"tr",align:"right"},"86.57"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.78"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.37")))),(0,n.kt)("h2",{id:"cost-of-fanout-transaction"},"Cost of FanOut Transaction"),(0,n.kt)("p",null,"Involves spending head output and burning head tokens. Uses ada-only UTxO for better comparability."),(0,n.kt)("table",null,(0,n.kt)("thead",{parentName:"table"},(0,n.kt)("tr",{parentName:"thead"},(0,n.kt)("th",{parentName:"tr",align:"left"},"Parties"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO"),(0,n.kt)("th",{parentName:"tr",align:"left"},"UTxO (bytes)"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Tx size"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max Mem"),(0,n.kt)("th",{parentName:"tr",align:"right"},"% max CPU"),(0,n.kt)("th",{parentName:"tr",align:"right"},"Min fee \u20b3"))),(0,n.kt)("tbody",{parentName:"table"},(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"0"),(0,n.kt)("td",{parentName:"tr",align:"left"},"0"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4765"),(0,n.kt)("td",{parentName:"tr",align:"right"},"8.72"),(0,n.kt)("td",{parentName:"tr",align:"right"},"3.59"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.46")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1"),(0,n.kt)("td",{parentName:"tr",align:"left"},"56"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4800"),(0,n.kt)("td",{parentName:"tr",align:"right"},"10.12"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4.41"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.48")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"285"),(0,n.kt)("td",{parentName:"tr",align:"right"},"4946"),(0,n.kt)("td",{parentName:"tr",align:"right"},"15.71"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7.72"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.55")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"10"),(0,n.kt)("td",{parentName:"tr",align:"left"},"568"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5124"),(0,n.kt)("td",{parentName:"tr",align:"right"},"22.69"),(0,n.kt)("td",{parentName:"tr",align:"right"},"11.84"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.64")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"20"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1139"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5485"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.67"),(0,n.kt)("td",{parentName:"tr",align:"right"},"20.11"),(0,n.kt)("td",{parentName:"tr",align:"right"},"0.83")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"30"),(0,n.kt)("td",{parentName:"tr",align:"left"},"1710"),(0,n.kt)("td",{parentName:"tr",align:"right"},"5848"),(0,n.kt)("td",{parentName:"tr",align:"right"},"50.65"),(0,n.kt)("td",{parentName:"tr",align:"right"},"28.38"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.02")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"40"),(0,n.kt)("td",{parentName:"tr",align:"left"},"2274"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6202"),(0,n.kt)("td",{parentName:"tr",align:"right"},"64.63"),(0,n.kt)("td",{parentName:"tr",align:"right"},"36.65"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.21")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"50"),(0,n.kt)("td",{parentName:"tr",align:"left"},"2846"),(0,n.kt)("td",{parentName:"tr",align:"right"},"6565"),(0,n.kt)("td",{parentName:"tr",align:"right"},"78.62"),(0,n.kt)("td",{parentName:"tr",align:"right"},"44.92"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.40")),(0,n.kt)("tr",{parentName:"tbody"},(0,n.kt)("td",{parentName:"tr",align:"left"},"5"),(0,n.kt)("td",{parentName:"tr",align:"left"},"65"),(0,n.kt)("td",{parentName:"tr",align:"left"},"3703"),(0,n.kt)("td",{parentName:"tr",align:"right"},"7105"),(0,n.kt)("td",{parentName:"tr",align:"right"},"99.62"),(0,n.kt)("td",{parentName:"tr",align:"right"},"57.34"),(0,n.kt)("td",{parentName:"tr",align:"right"},"1.68")))))}N.isMDXComponent=!0}}]);