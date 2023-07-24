"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[7174],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>m});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function p(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},o=Object.keys(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(r=0;r<o.length;r++)n=o[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var l=r.createContext({}),s=function(e){var t=r.useContext(l),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},d=function(e){var t=s(e.components);return r.createElement(l.Provider,{value:t},e.children)},u="mdxType",c={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},h=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,o=e.originalType,l=e.parentName,d=p(e,["components","mdxType","originalType","parentName"]),u=s(n),h=a,m=u["".concat(l,".").concat(h)]||u[h]||c[h]||o;return n?r.createElement(m,i(i({ref:t},d),{},{components:n})):r.createElement(m,i({ref:t},d))}));function m(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var o=n.length,i=new Array(o);i[0]=h;var p={};for(var l in t)hasOwnProperty.call(t,l)&&(p[l]=t[l]);p.originalType=e,p[u]="string"==typeof e?e:a,i[1]=p;for(var s=2;s<o;s++)i[s]=n[s];return r.createElement.apply(null,i)}return r.createElement.apply(null,n)}h.displayName="MDXCreateElement"},69376:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>c,frontMatter:()=>o,metadata:()=>p,toc:()=>s});var r=n(87462),a=(n(67294),n(3905));const o={sidebar_position:4},i="API Behavior",p={unversionedId:"behavior",id:"behavior",title:"API Behavior",description:"This page documents the behavior of a hydra-node at the API layer. That is, how the system behaves given ClientInputs and what ServerOutputs are produced in response to it. See also the API reference for more details about individual API messages.",source:"@site/core-concepts/behavior.md",sourceDirName:".",slug:"/behavior",permalink:"/head-protocol/ja/core-concepts/behavior",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/core-concepts/behavior.md",tags:[],version:"current",sidebarPosition:4,frontMatter:{sidebar_position:4},sidebar:"defaultSidebar",previous:{title:"\u30ec\u30a4\u30e4\u30fc2\u30bd\u30ea\u30e5\u30fc\u30b7\u30e7\u30f3",permalink:"/head-protocol/ja/core-concepts/layer-two"},next:{title:"\u30ed\u30fc\u30eb\u30d0\u30c3\u30af\u51e6\u7406",permalink:"/head-protocol/ja/core-concepts/rollbacks/"}},l={},s=[{value:"API configuration",id:"api-configuration",level:4},{value:"Replay of past server outputs",id:"replay-of-past-server-outputs",level:2}],d={toc:s},u="wrapper";function c(e){let{components:t,...n}=e;return(0,a.kt)(u,(0,r.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"api-behavior"},"API Behavior"),(0,a.kt)("p",null,"This page documents the behavior of a ",(0,a.kt)("inlineCode",{parentName:"p"},"hydra-node")," at the API layer. That is, how the system behaves given ",(0,a.kt)("a",{parentName:"p",href:"/haddock/hydra-node/Hydra-API-ClientInput.html#t:ClientInput"},"ClientInputs")," and what ",(0,a.kt)("a",{parentName:"p",href:"/haddock/hydra-node/Hydra-API-ServerOutput.html#t:ServerOutput"},"ServerOutputs")," are produced in response to it. See also the ",(0,a.kt)("a",{parentName:"p",href:"/api-reference/"},"API reference")," for more details about individual API messages."),(0,a.kt)("p",null,"The formalism uses ",(0,a.kt)("a",{parentName:"p",href:"https://en.wikipedia.org/wiki/UML_state_machine"},"UML statechart")," language where transitions are labeled: ",(0,a.kt)("inlineCode",{parentName:"p"},"input [condition] / output"),". When two outputs (e.g. ",(0,a.kt)("inlineCode",{parentName:"p"},"A")," and ",(0,a.kt)("inlineCode",{parentName:"p"},"B"),") are expected we write ",(0,a.kt)("inlineCode",{parentName:"p"},"A,B"),", while ",(0,a.kt)("inlineCode",{parentName:"p"},"{A,B}")," denotes mutual exclusiveness of outputs."),(0,a.kt)("p",null,(0,a.kt)("img",{parentName:"p",src:"https://www.plantuml.com/plantuml/svg/ZP4_J_Cm48Vt-nIUNpLwzmweA18Y2o28MAeEBdUX9zddABv02kAxOzVGG65Wkp_FSr5-NaMrWuxUmOwH3FbUzmOfFWpSAmS1MF_RcAewCusmidNyml9ebeVM_3UtP77VXZfupmhm3Vef5InffL324oCf5opM9VPy6uQCNfB59kRkL_ow9qdq9vTRWCDmNbxHSpibvBMTxfEBYtPgv2bNRuixiNtS1Qs3T3numYBdqxKBaT0iIt7yH1a3VAo_W3CIH2aguH7AFsObZ7eJOkIjeqIlbJsPtiP8qTXEQJ2OTCT2cpdQeW6Sw9MZJkUuolKvhpJEAH425ABmVtl65GcEthq3",alt:null})),(0,a.kt)("p",null,(0,a.kt)("a",{parentName:"p",href:"https://www.plantuml.com/plantuml/uml/ZP6zJWCn383tF8Ldr8gz0oggG7G112UMgaEANMefSKw92zSYtftaKbrBOM3pp_UTVSuqgOswzSvi60d8jxe3fFGQkLKEOipYOWdZyHNCXPMjuptB6qpw52xXYIpkcZ0BthCQymFwBLKiQpLGmZCZAxSircNsUXLYGU_8uZoLx4_yeIM1oS2Lr9Y-U6pUqqVJPUIpwLwoYwEccylx8RhfMew4NwDdiMtQ19q5MNFqXFKpPSZCXKG8aneCmshPc4Fx51oG84f92GUe_AALiDN7a1Al76LweUm9MfiI9R1hfGYOTST2o-EHgsRCcJvsjDQJmzNsi45VvdtPinAL_z-3Jabmwzqt"},"Edit this diagram")),(0,a.kt)("p",null,"Not pictured is the ",(0,a.kt)("inlineCode",{parentName:"p"},"CommandFailed")," output, which is implicit emitted whenever an input is used when no transition below applies. Also non-state-changing or life-cycle relevant inputs like ",(0,a.kt)("inlineCode",{parentName:"p"},"GetUTxO")," are not mentioned, as well as outputs like ",(0,a.kt)("inlineCode",{parentName:"p"},"Greetings"),", ",(0,a.kt)("inlineCode",{parentName:"p"},"InvalidInput"),", ",(0,a.kt)("inlineCode",{parentName:"p"},"PeerConnected"),", ",(0,a.kt)("inlineCode",{parentName:"p"},"PeerDisconnected")," and ",(0,a.kt)("inlineCode",{parentName:"p"},"GetUTxOResponse"),"."),(0,a.kt)("h4",{id:"api-configuration"},"API configuration"),(0,a.kt)("p",null,"There are some options for API clients to control the server outputs. Server outputs are controlled using the following query parameters:"),(0,a.kt)("ul",null,(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"history=no")," -> Prevents historical outputs display. All server outputs are recorded and when a client re-connects these outputs are replayed unless ",(0,a.kt)("inlineCode",{parentName:"li"},"history=no")," query param is used."),(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"tx-output=cbor")," -> Outputs transaction fields encoded as CBOR instead of default JSON."),(0,a.kt)("li",{parentName:"ul"},(0,a.kt)("inlineCode",{parentName:"li"},"snapshot-utxo=no")," -> In case of a ",(0,a.kt)("inlineCode",{parentName:"li"},"SnapshotConfirmed")," message the ",(0,a.kt)("inlineCode",{parentName:"li"},"utxo")," field in the inner ",(0,a.kt)("inlineCode",{parentName:"li"},"Snapshot")," will be omitted.")),(0,a.kt)("h2",{id:"replay-of-past-server-outputs"},"Replay of past server outputs"),(0,a.kt)("p",null,"When a ",(0,a.kt)("inlineCode",{parentName:"p"},"hydra-node")," restarts, by default it will load it's history from persistence and replay previous server outputs to enable clients to re-establish their state upon re-connection. If that happens, obviously some of these outputs are not relevant anymore. One example of this is the ",(0,a.kt)("inlineCode",{parentName:"p"},"PeerConnected")," and ",(0,a.kt)("inlineCode",{parentName:"p"},"PeerDisconnected"),". To make it possible to determine the end of replayed history, client applications can use the ",(0,a.kt)("inlineCode",{parentName:"p"},"Greetings"),", which will be emitted on every ",(0,a.kt)("inlineCode",{parentName:"p"},"hydra-node")," start. See the ",(0,a.kt)("inlineCode",{parentName:"p"},"hydra-tui")," example client for how this is handled."),(0,a.kt)("p",null,"Clients can optionally decide to skip history outputs and receive only the ",(0,a.kt)("inlineCode",{parentName:"p"},"Greetings")," and following ones. In order to do that they can use query param ",(0,a.kt)("inlineCode",{parentName:"p"},"history=no"),"."),(0,a.kt)("p",null,"For example if the client wants to connect to a local ",(0,a.kt)("inlineCode",{parentName:"p"},"hydra-node")," and doesn't want to view the server history but also want to have the transactions encoded as CBOR (base16) and prevent utxo display in ",(0,a.kt)("inlineCode",{parentName:"p"},"SnapshotConfirmed")," messages, they would connect using default port ",(0,a.kt)("inlineCode",{parentName:"p"},"4001")," and the full path ",(0,a.kt)("inlineCode",{parentName:"p"},"ws://localhost:4001/?history=no&tx-output=cbor"),"."))}c.isMDXComponent=!0}}]);