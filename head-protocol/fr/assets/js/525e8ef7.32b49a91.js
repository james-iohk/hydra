"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[2037],{19257:(e,t,n)=>{n.d(t,{Z:()=>i});var a=n(67294),r=n(60614);const o={terminalWindow:"terminalWindow_wGrl",terminalWindowHeader:"terminalWindowHeader_o9Cs",terminalWindowBody:"terminalWindowBody_tzdS",row:"row_Rn7G",buttons:"buttons_IGLB",right:"right_fWp9",dot:"dot_fGZE"};function i(e){let{children:t,minHeight:n}=e;const i="string"==typeof t?a.createElement(r.Z,null,t):t;return a.createElement("div",{className:o.terminalWindow,style:{minHeight:n}},a.createElement("div",{className:o.terminalWindowHeader},a.createElement("div",{className:o.buttons},a.createElement("span",{className:o.dot,style:{background:"#f25f58"}}),a.createElement("span",{className:o.dot,style:{background:"#fbbe3c"}}),a.createElement("span",{className:o.dot,style:{background:"#58cb42"}}))),a.createElement("div",{className:o.terminalWindowBody},i))}},1083:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>S,contentTitle:()=>_,default:()=>Z,frontMatter:()=>I,metadata:()=>E,toc:()=>D});var a=n(87462),r=n(67294),o=n(3905),i=n(19257),l=n(86010),d=n(12466),s=n(16550),c=n(91980),u=n(67392),p=n(50012);function m(e){return function(e){return r.Children.map(e,(e=>{if(!e||(0,r.isValidElement)(e)&&function(e){const{props:t}=e;return!!t&&"object"==typeof t&&"value"in t}(e))return e;throw new Error(`Docusaurus error: Bad <Tabs> child <${"string"==typeof e.type?e.type:e.type.name}>: all children of the <Tabs> component should be <TabItem>, and every <TabItem> should have a unique "value" prop.`)}))?.filter(Boolean)??[]}(e).map((e=>{let{props:{value:t,label:n,attributes:a,default:r}}=e;return{value:t,label:n,attributes:a,default:r}}))}function k(e){const{values:t,children:n}=e;return(0,r.useMemo)((()=>{const e=t??m(n);return function(e){const t=(0,u.l)(e,((e,t)=>e.value===t.value));if(t.length>0)throw new Error(`Docusaurus error: Duplicate values "${t.map((e=>e.value)).join(", ")}" found in <Tabs>. Every value needs to be unique.`)}(e),e}),[t,n])}function h(e){let{value:t,tabValues:n}=e;return n.some((e=>e.value===t))}function y(e){let{queryString:t=!1,groupId:n}=e;const a=(0,s.k6)(),o=function(e){let{queryString:t=!1,groupId:n}=e;if("string"==typeof t)return t;if(!1===t)return null;if(!0===t&&!n)throw new Error('Docusaurus error: The <Tabs> component groupId prop is required if queryString=true, because this value is used as the search param name. You can also provide an explicit value such as queryString="my-search-param".');return n??null}({queryString:t,groupId:n});return[(0,c._X)(o),(0,r.useCallback)((e=>{if(!o)return;const t=new URLSearchParams(a.location.search);t.set(o,e),a.replace({...a.location,search:t.toString()})}),[o,a])]}function v(e){const{defaultValue:t,queryString:n=!1,groupId:a}=e,o=k(e),[i,l]=(0,r.useState)((()=>function(e){let{defaultValue:t,tabValues:n}=e;if(0===n.length)throw new Error("Docusaurus error: the <Tabs> component requires at least one <TabItem> children component");if(t){if(!h({value:t,tabValues:n}))throw new Error(`Docusaurus error: The <Tabs> has a defaultValue "${t}" but none of its children has the corresponding value. Available values are: ${n.map((e=>e.value)).join(", ")}. If you intend to show no default tab, use defaultValue={null} instead.`);return t}const a=n.find((e=>e.default))??n[0];if(!a)throw new Error("Unexpected error: 0 tabValues");return a.value}({defaultValue:t,tabValues:o}))),[d,s]=y({queryString:n,groupId:a}),[c,u]=function(e){let{groupId:t}=e;const n=function(e){return e?`docusaurus.tab.${e}`:null}(t),[a,o]=(0,p.Nk)(n);return[a,(0,r.useCallback)((e=>{n&&o.set(e)}),[n,o])]}({groupId:a}),m=(()=>{const e=d??c;return h({value:e,tabValues:o})?e:null})();(0,r.useLayoutEffect)((()=>{m&&l(m)}),[m]);return{selectedValue:i,selectValue:(0,r.useCallback)((e=>{if(!h({value:e,tabValues:o}))throw new Error(`Can't select invalid tab value=${e}`);l(e),s(e),u(e)}),[s,u,o]),tabValues:o}}var g=n(72389);const b={tabList:"tabList__CuJ",tabItem:"tabItem_LNqP"};function f(e){let{className:t,block:n,selectedValue:o,selectValue:i,tabValues:s}=e;const c=[],{blockElementScrollPositionUntilNextRender:u}=(0,d.o5)(),p=e=>{const t=e.currentTarget,n=c.indexOf(t),a=s[n].value;a!==o&&(u(t),i(a))},m=e=>{let t=null;switch(e.key){case"Enter":p(e);break;case"ArrowRight":{const n=c.indexOf(e.currentTarget)+1;t=c[n]??c[0];break}case"ArrowLeft":{const n=c.indexOf(e.currentTarget)-1;t=c[n]??c[c.length-1];break}}t?.focus()};return r.createElement("ul",{role:"tablist","aria-orientation":"horizontal",className:(0,l.Z)("tabs",{"tabs--block":n},t)},s.map((e=>{let{value:t,label:n,attributes:i}=e;return r.createElement("li",(0,a.Z)({role:"tab",tabIndex:o===t?0:-1,"aria-selected":o===t,key:t,ref:e=>c.push(e),onKeyDown:m,onClick:p},i,{className:(0,l.Z)("tabs__item",b.tabItem,i?.className,{"tabs__item--active":o===t})}),n??t)})))}function w(e){let{lazy:t,children:n,selectedValue:a}=e;const o=(Array.isArray(n)?n:[n]).filter(Boolean);if(t){const e=o.find((e=>e.props.value===a));return e?(0,r.cloneElement)(e,{className:"margin-top--md"}):null}return r.createElement("div",{className:"margin-top--md"},o.map(((e,t)=>(0,r.cloneElement)(e,{key:t,hidden:e.props.value!==a}))))}function N(e){const t=v(e);return r.createElement("div",{className:(0,l.Z)("tabs-container",b.tabList)},r.createElement(f,(0,a.Z)({},e,t)),r.createElement(w,(0,a.Z)({},e,t)))}function T(e){const t=(0,g.Z)();return r.createElement(N,(0,a.Z)({key:String(t)},e))}const C={tabItem:"tabItem_Ymn6"};function x(e){let{children:t,hidden:n,className:a}=e;return r.createElement("div",{role:"tabpanel",className:(0,l.Z)(C.tabItem,a),hidden:n},t)}const I={sidebar_position:3},_="Without Docker",E={unversionedId:"getting-started/demo/without-docker",id:"getting-started/demo/without-docker",title:"Without Docker",description:"Running the demo without Docker containers, but with plain executables and scripts.",source:"@site/docs/getting-started/demo/without-docker.md",sourceDirName:"getting-started/demo",slug:"/getting-started/demo/without-docker",permalink:"/head-protocol/fr/docs/getting-started/demo/without-docker",draft:!1,editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/docs/getting-started/demo/without-docker.md",tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_position:3},sidebar:"defaultSidebar",previous:{title:"Via Docker",permalink:"/head-protocol/fr/docs/getting-started/demo/with-docker"},next:{title:"Operating a Hydra Node",permalink:"/head-protocol/fr/docs/getting-started/operating-hydra"}},S={},D=[{value:"Preparation",id:"preparation",level:2},{value:"Setting-up The Chain",id:"setting-up-the-chain",level:2},{value:"Seeding The Network",id:"seeding-the-network",level:2},{value:"Setting-up The Hydra Network",id:"setting-up-the-hydra-network",level:2},{value:"Running The Clients",id:"running-the-clients",level:2}],W={toc:D},V="wrapper";function Z(e){let{components:t,...n}=e;return(0,o.kt)(V,(0,a.Z)({},W,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"without-docker"},"Without Docker"),(0,o.kt)("blockquote",null,(0,o.kt)("p",{parentName:"blockquote"},"Running the demo without Docker containers, but with plain executables and scripts.")),(0,o.kt)("h2",{id:"preparation"},"Preparation"),(0,o.kt)("p",null,"Make sure that you have a ",(0,o.kt)("inlineCode",{parentName:"p"},"cardano-node"),", ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-node")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"hydra-tui")," executable in your scope. You can either"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"use ",(0,o.kt)("inlineCode",{parentName:"li"},"nix develop .#demo")," or"),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("inlineCode",{parentName:"li"},"cabal build")," and ",(0,o.kt)("inlineCode",{parentName:"li"},"cabal exec")," them (do not forget the ",(0,o.kt)("inlineCode",{parentName:"li"},"--")," before passing further arguments).")),(0,o.kt)("admonition",{title:"Tip for tmux users",type:"info"},(0,o.kt)("p",{parentName:"admonition"},"In the ",(0,o.kt)("inlineCode",{parentName:"p"},"demo")," nix shell, there is a ",(0,o.kt)("inlineCode",{parentName:"p"},"run-tmux")," script which starts a new ",(0,o.kt)("inlineCode",{parentName:"p"},"tmux")," session with multiple windows and panes executing all the commands below!")),(0,o.kt)("p",null,"All further commands are written as if executed from the ",(0,o.kt)("inlineCode",{parentName:"p"},"demo")," folder in the project repository, so make sure to ",(0,o.kt)("inlineCode",{parentName:"p"},"cd demo")," before continuing."),(0,o.kt)("admonition",{title:"Tip for nix-direnv users",type:"info"},(0,o.kt)("p",{parentName:"admonition"},"Allowing the ",(0,o.kt)("inlineCode",{parentName:"p"},"demo/.envrc")," will ensure you have the nix shell environment available whenever you are in the ",(0,o.kt)("inlineCode",{parentName:"p"},"demo/")," directory. To use this, opt-in via ",(0,o.kt)("inlineCode",{parentName:"p"},"direnv allow")," after ",(0,o.kt)("inlineCode",{parentName:"p"},"cd demo"),".")),(0,o.kt)("h2",{id:"setting-up-the-chain"},"Setting-up The Chain"),(0,o.kt)("p",null,"First, let's prepare and start an ad-hoc, single ",(0,o.kt)("inlineCode",{parentName:"p"},"cardano-node")," devnet using our configuration. Note that this will create a ",(0,o.kt)("inlineCode",{parentName:"p"},"devnet")," directory in your current working directory:"),(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"./prepare-devnet.sh\ncd devnet\ncardano-node run \\\n  --config cardano-node.json \\\n  --topology topology.json \\\n  --database-path db \\\n  --socket-path node.socket \\\n  --shelley-operational-certificate opcert.cert \\\n  --shelley-kes-key kes.skey \\\n  --shelley-vrf-key vrf.skey\n"))),(0,o.kt)("h2",{id:"seeding-the-network"},"Seeding The Network"),(0,o.kt)("p",null,"From the ",(0,o.kt)("inlineCode",{parentName:"p"},"demo")," folder you can use the ",(0,o.kt)("inlineCode",{parentName:"p"},"seed-devnet.sh")," script by passing it the path/command to a cardano-cli and hydra-node executable to use, instead of having it using the Docker container. For example:"),(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"export CARDANO_NODE_SOCKET_PATH=devnet/node.socket\n./seed-devnet.sh $(which cardano-cli) $(which hydra-node)\n"))),(0,o.kt)("p",null,"Note, should you want to use ",(0,o.kt)("inlineCode",{parentName:"p"},"cabal"),", pass the invocation for example like this ",(0,o.kt)("inlineCode",{parentName:"p"},'"cabal exec hydra-node --"'),"."),(0,o.kt)("h2",{id:"setting-up-the-hydra-network"},"Setting-up The Hydra Network"),(0,o.kt)("p",null,"Then, in 3 different terminals, start 3 Hydra nodes from the ",(0,o.kt)("inlineCode",{parentName:"p"},"demo/")," directory:"),(0,o.kt)("admonition",{title:"Note",type:"info"},(0,o.kt)("p",{parentName:"admonition"},"We are trying to force ipv4 addresses by using ",(0,o.kt)("inlineCode",{parentName:"p"},"--peer 127.0.0.1"),".\nIf you don't see any connected peers in the tui it probably means that your system is configured to use ipv6.")),(0,o.kt)(T,{mdxType:"Tabs"},(0,o.kt)(x,{value:"Alice",mdxType:"TabItem"},(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"source .env && hydra-node \\\n  --node-id 1 --port 5001 --api-port 4001 --monitoring-port 6001 \\\n  --peer 127.0.0.1:5002 \\\n  --peer 127.0.0.1:5003 \\\n  --hydra-signing-key alice.sk \\\n  --hydra-verification-key bob.vk \\\n  --hydra-verification-key carol.vk \\\n  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \\\n  --cardano-signing-key devnet/credentials/alice.sk \\\n  --cardano-verification-key devnet/credentials/bob.vk \\\n  --cardano-verification-key devnet/credentials/carol.vk \\\n  --ledger-protocol-parameters devnet/protocol-parameters.json \\\n  --testnet-magic 42 \\\n  --node-socket devnet/node.socket \\\n  --persistence-dir devnet/persistence/alice\n")))),(0,o.kt)(x,{value:"Bob",mdxType:"TabItem"},(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"source .env && hydra-node \\\n  --node-id 2 --port 5002 --api-port 4002 --monitoring-port 6002 \\\n  --peer 127.0.0.1:5001 \\\n  --peer 127.0.0.1:5003 \\\n  --hydra-signing-key bob.sk \\\n  --hydra-verification-key alice.vk \\\n  --hydra-verification-key carol.vk \\\n  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \\\n  --cardano-signing-key devnet/credentials/bob.sk \\\n  --cardano-verification-key devnet/credentials/alice.vk \\\n  --cardano-verification-key devnet/credentials/carol.vk \\\n  --ledger-protocol-parameters devnet/protocol-parameters.json \\\n  --testnet-magic 42 \\\n  --node-socket devnet/node.socket \\\n  --persistence-dir devnet/persistence/bob\n")))),(0,o.kt)(x,{value:"Carol",mdxType:"TabItem"},(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"source .env && hydra-node \\\n  --node-id 3 --port 5003 --api-port 4003 --monitoring-port 6003 \\\n  --peer 127.0.0.1:5001 \\\n  --peer 127.0.0.1:5002 \\\n  --hydra-signing-key carol.sk \\\n  --hydra-verification-key alice.vk \\\n  --hydra-verification-key bob.vk \\\n  --hydra-scripts-tx-id $HYDRA_SCRIPTS_TX_ID \\\n  --cardano-signing-key devnet/credentials/carol.sk \\\n  --cardano-verification-key devnet/credentials/alice.vk \\\n  --cardano-verification-key devnet/credentials/bob.vk \\\n  --ledger-protocol-parameters devnet/protocol-parameters.json \\\n  --testnet-magic 42 \\\n  --node-socket devnet/node.socket \\\n  --persistence-dir devnet/persistence/carol\n"))))),(0,o.kt)("p",null,"If things go well, the nodes should start logging once connected to the chain."),(0,o.kt)("h2",{id:"running-the-clients"},"Running The Clients"),(0,o.kt)("p",null,"Connect to the nodes using hydra-tui."),(0,o.kt)(T,{mdxType:"Tabs"},(0,o.kt)(x,{value:"Alice",mdxType:"TabItem"},(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"hydra-tui \\\n  --connect 0.0.0.0:4001 \\\n  --cardano-signing-key devnet/credentials/alice-funds.sk \\\n  --testnet-magic 42 \\\n  --node-socket devnet/node.socket\n")))),(0,o.kt)(x,{value:"Bob",mdxType:"TabItem"},(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"hydra-tui \\\n  --connect 0.0.0.0:4002 \\\n  --cardano-signing-key devnet/credentials/bob-funds.sk \\\n  --testnet-magic 42 \\\n  --node-socket devnet/node.socket\n")))),(0,o.kt)(x,{value:"Carol",mdxType:"TabItem"},(0,o.kt)(i.Z,{mdxType:"TerminalWindow"},(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre"},"hydra-tui \\\n  --connect 0.0.0.0:4003 \\\n  --cardano-signing-key devnet/credentials/carol-funds.sk \\\n  --testnet-magic 42 \\\n  --node-socket devnet/node.socket\n"))))))}Z.isMDXComponent=!0}}]);