"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[7172],{3905:(e,t,a)=>{a.d(t,{Zo:()=>h,kt:()=>c});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function o(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?o(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):o(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},o=Object.keys(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(n=0;n<o.length;n++)a=o[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var p=n.createContext({}),l=function(e){var t=n.useContext(p),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},h=function(e){var t=l(e.components);return n.createElement(p.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,o=e.originalType,p=e.parentName,h=s(e,["components","mdxType","originalType","parentName"]),u=l(a),m=r,c=u["".concat(p,".").concat(m)]||u[m]||d[m]||o;return a?n.createElement(c,i(i({ref:t},h),{},{components:a})):n.createElement(c,i({ref:t},h))}));function c(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=a.length,i=new Array(o);i[0]=m;var s={};for(var p in t)hasOwnProperty.call(t,p)&&(s[p]=t[p]);s.originalType=e,s[u]="string"==typeof e?e:r,i[1]=s;for(var l=2;l<o;l++)i[l]=a[l];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}m.displayName="MDXCreateElement"},12837:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>p,contentTitle:()=>i,default:()=>d,frontMatter:()=>o,metadata:()=>s,toc:()=>l});var n=a(87462),r=(a(67294),a(3905));const o={title:"March 2023",slug:"2023-03",authors:["ffakenz","v0d1ch","ch1bo"],tags:["monthly"]},i=void 0,s={permalink:"/head-protocol/ja/monthly/2023-03",editUrl:"https://github.com/input-output-hk/hydra/tree/master/docs/monthly/2023-03-monthly.md",source:"@site/monthly/2023-03-monthly.md",title:"March 2023",description:"This report summarizes the work on Hydra since February 2023. It serves as",date:"2023-04-04T13:15:50.000Z",formattedDate:"2023\u5e744\u67084\u65e5",tags:[{label:"monthly",permalink:"/head-protocol/ja/monthly/tags/monthly"}],readingTime:5.03,hasTruncateMarker:!1,authors:[{name:"Franco Testagrossa",title:"Senior software engineer - Hydra @ IOG",url:"https://github.com/ffakenz",imageURL:"https://github.com/ffakenz.png",key:"ffakenz"},{name:"Sasha Bogicevic",title:"Senior software engineer - Hydra @ IOG",url:"https://github.com/v0d1ch",imageURL:"https://github.com/v0d1ch.png",key:"v0d1ch"},{name:"Sebastian Nagel",title:"Software Engineering Lead - Hydra @ IOG",url:"https://github.com/ch1bo",imageURL:"https://github.com/ch1bo.png",key:"ch1bo"}],frontMatter:{title:"March 2023",slug:"2023-03",authors:["ffakenz","v0d1ch","ch1bo"],tags:["monthly"]},prevItem:{title:"April 2023",permalink:"/head-protocol/ja/monthly/2023-04"},nextItem:{title:"February 2023",permalink:"/head-protocol/ja/monthly/2023-02"}},p={authorsImageUrls:[void 0,void 0,void 0]},l=[{value:"Roadmap",id:"roadmap",level:2},{value:"Released version 0.9.0",id:"released-version-090",level:4},{value:"Notable roadmap updates",id:"notable-roadmap-updates",level:4},{value:"Development",id:"development",level:2},{value:"Community",id:"community",level:2},{value:"Conclusion",id:"conclusion",level:2}],h={toc:l},u="wrapper";function d(e){let{components:t,...o}=e;return(0,r.kt)(u,(0,n.Z)({},h,o,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("p",null,"This report summarizes the work on Hydra since February 2023. It serves as\npreparation for the monthly review meeting\n(",(0,r.kt)("a",{parentName:"p",href:"https://docs.google.com/presentation/d/1yZ4AqUQ8OBMG9ARMYvj3IOjaIAqglf7kZei4vsLMrbs/edit#slide=id.g1f87a7454a5_0_1392"},"slides"),"/",(0,r.kt)("a",{parentName:"p",href:"https://www.youtube.com/watch?v=mA9lMV0tKN8"},"recording"),")\n, where the team updates major project stakeholders on recent developments to\ngather their feedback on proposed plans."),(0,r.kt)("h2",{id:"roadmap"},"Roadmap"),(0,r.kt)("p",null,"The project saw one release this month and several items on our\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/orgs/input-output-hk/projects/21"},"roadmap")," were updated."),(0,r.kt)("h4",{id:"released-version-090"},"Released version 0.9.0"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"This release brought in on-chain and off-chain changes that are now also fully\nreflected in our specification.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"We managed to decrease the costs of our plutus scripts by using a new error code\nframework and made also the head script a reference script.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"The mutation test suite is improved. Now, if tests fail they show the correct reason.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"The contestation deadline is now pushed out by each contesting party, so the\ncontestation period parameter can be chosen irrespective of number of parties.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Added a tutorial contributed by ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/perturbing/"},"@perturbing"),".")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/releases/tag/0.9.0"},"Full release notes")," and list of ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/milestone/9?closed=1"},"delivered features")))),(0,r.kt)("p",null,(0,r.kt)("img",{src:a(65956).Z,width:"1873",height:"609"})," ",(0,r.kt)("small",null,(0,r.kt)("center",null,"The latest roadmap, with many items marked as idea."))),(0,r.kt)("h4",{id:"notable-roadmap-updates"},"Notable roadmap updates"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Now that 0.9.0 is released, the focus is on mainnet compatiblity. The\nnext planned version ",(0,r.kt)("strong",{parentName:"p"},"0.10.0")," will be the first ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," to be\nmainnet compatible.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Also prioritized ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/380"},"API configurability\n#380")," higher and planned\nit into 0.10.0 as more users were requesting this.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"We aim for several more ",(0,r.kt)("strong",{parentName:"p"},"0.x.0")," versions to incorporate user-requested\nfeatures before reaching a 1.0.0, which will be a fully maintained release\nwhere features can be considered stable and won't be removed without\nappropriate deprecation cycles.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"Marked multiple features as \ud83d\udcad ",(0,r.kt)("em",{parentName:"p"},"idea"),", meaning they are up for discussion.\nMany have been on the roadmap for a long time without user demand or input,\nwhile other ideas are incubating as ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/discussions/categories/ideas"},"github idea\ndiscussion"),".\nWe are planning to convert these idea issues to GitHub discussions as these\nprovide better dicussion tools and allow to gauge interest in topics. In turn,\nby cleaning up we provide more room for the most popular ideas to be planned\nonto the roadmap in a more timely manner.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"In short, if you want to see some feature added, ",(0,r.kt)("strong",{parentName:"p"},"show your support")," on the\ncorresponding ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/discussions/categories/ideas"},"idea\ndiscussion"),".")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},"The \ud83d\udcac ",(0,r.kt)("em",{parentName:"p"},"feature"),' items remaining are the currently identifed "must-haves" or\ntoo vague to remove \ud83d\udd34 ',(0,r.kt)("em",{parentName:"p"},"red")," items."))),(0,r.kt)("p",null,(0,r.kt)("img",{src:a(74095).Z,width:"1778",height:"749"})," ",(0,r.kt)("small",null,(0,r.kt)("center",null,"The roadmap without idea items."))),(0,r.kt)("h2",{id:"development"},"Development"),(0,r.kt)("p",null,(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues?q=is%3Aclosed+sort%3Aupdated-desc+closed%3A2023-02-24..2023-03-29"},"Issues and pull requests closed since last\nreport")),(0,r.kt)("p",null,"This month, the team worked on the following:"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Making Hydra mainnet compatible.")," Besides making it technically possible to\nrun on mainnet, this is about safeguarding our users and preventing them from\nshooting themselves in the foot with a mainnet gun. That is why we\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/762"},"limited")," the amount of\nada you can commit to a head on mainnet. Our smoke tests should be running on\nmainnet also so we made sure to\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/770"},"return")," the leftover funds\nback to our faucet. There was also\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/775"},"work")," on our CI that\nenables running the tests on mainnet using a dedicated github runner.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Improving the Hydra UX.")," We noticed a possible pitfall when restarting the\n",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," using different parameters than in the persisted state. Now, the\nnode would ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/764"},"prevent")," this\nkind of misconfiguration.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Optimize the on-chain scripts.")," Reduced the cost of opening/closing a Head\nby reducing size of scripts via error codes\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/748"},"#748")," and also having the\nhead script as a reference only\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/701"},"#701"),")."),(0,r.kt)("p",{parentName:"li"},"The mutation test also got improved by making all cases expect the\ncorresponding error codes\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/issues/705"},"#705")," and new ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/772"},"golden\ntest suite ")," ensures the\nscript hashes don't change accidentally."),(0,r.kt)("p",{parentName:"li"},"Furthermore, we addressed a problem discovered in our property tests\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/724"},"#724")," by preventing\ncommitting outputs with reference scripts to a Head\n",(0,r.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/pull/766"},"#766"),". This is still a\ndocumented known issue, but a workaround is available."))),(0,r.kt)("h2",{id:"community"},"Community"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Team workshop in Austria.")," Meetup of core contributors in Feldkirch,\nAustria for retrospective, grooming and discussions on future plans for\nHydra. Part of the agenda was also a presentation of Hydra to members of the\nCardano Foundation and a meeting with builders from the Cardano community."),(0,r.kt)("p",{parentName:"li"},"The highlight, though was the common effort of going the last mile to open a\nhead on mainnet for the monthly review meeting! \ud83c\udf89"),(0,r.kt)("blockquote",{class:"twitter-tweet"},(0,r.kt)("p",{lang:"en",dir:"ltr"},"JUST IN: We have a Hydra Head live on the ",(0,r.kt)("a",{href:"https://twitter.com/hashtag/Cardano?src=hash&ref_src=twsrc%5Etfw"},"#Cardano")," Mainnet \ud83d\ude80 ",(0,r.kt)("a",{href:"https://t.co/6kDKq7T7no"},"pic.twitter.com/6kDKq7T7no")),"\u2014 Emmanuel \ud80c\udc80 \ud83c\udf55 \ud80c\udd53\ud83c\uddec\ud83c\udded\ud83e\udd84\ud83d\udfe3\u26a1\ufe0f (@thepizzaknight_) ",(0,r.kt)("a",{href:"https://twitter.com/thepizzaknight_/status/1638572527789252608?ref_src=twsrc%5Etfw"},"March 22, 2023"))," ",(0,r.kt)("script",{async:!0,src:"https://platform.twitter.com/widgets.js",charset:"utf-8"}),(0,r.kt)("p",{parentName:"li"},"We demonstrated our good old ",(0,r.kt)("inlineCode",{parentName:"p"},"hydraw")," application on that Hydra head and we\nsaw thousands of pixels painted by hundreds of users (no detailed metrics).")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Next step in Hydra for Payments.")," Also announced in the monthly meeting was\nthe next phase on the Hydra for Payments project. The scope of this will be to\nextend hydra-pay and build a mobile payment channels app that makes direct use\nof it - working title: HydraNow."))),(0,r.kt)("h2",{id:"conclusion"},"Conclusion"),(0,r.kt)("p",null,"The monthly review meeting for March was conducted on 2023-03-27 via Google\nMeet - ",(0,r.kt)("a",{parentName:"p",href:"https://docs.google.com/presentation/d/1yZ4AqUQ8OBMG9ARMYvj3IOjaIAqglf7kZei4vsLMrbs/edit#slide=id.g1f87a7454a5_0_1392"},"slides"),"/",(0,r.kt)("a",{parentName:"p",href:"https://www.youtube.com/watch?v=mA9lMV0tKN8"},"recording"),"."),(0,r.kt)("p",null,"This month was very important for the project and culminated in the first\ndemonstration of a Hydra Head on mainnet! The demo was well received and we\nreceived positive feedback in the meeting, on following twitter announcements\nand on the published recording on youtube. Inspired by this, we saw multiple\npeople offer to help and collaborate on communicating, educating and ultimately\nspreading the love. The invitation via the new discord category worked well - we\nhad about 40 community members in the call - and we will continue with this\nworkflow. Make sure to follow the Hydra\n",(0,r.kt)("a",{parentName:"p",href:"https://discord.gg/Bwdy2jNdm2"},"#announcements")," on the IOG Technical discord\nserver."),(0,r.kt)("p",null,"While being mainnet compatible is a major milestone for the project, there are\nstill many known issues, shortcomings and requested features. The roadmap\nchanges this month should make it clear that we are serious about the latter -\nHydra will only reach 1.0.0 if it is used by an application on mainnet. Hence,\nwe will focus on adding features required for payments, voting, auctions, ...\nand eventually, your use case."),(0,r.kt)("p",null,"Hydra Head is ready to be used on mainnet. Are you ready to use it?"))}d.isMDXComponent=!0},74095:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/2023-03-roadmap-ex-ideas-780befa80ce4acd0270b5952637d2265.png"},65956:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/2023-03-roadmap-0dbb5851443550bdeea79e517480eed0.png"}}]);