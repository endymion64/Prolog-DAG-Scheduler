%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          batch_large_homo.pl          %
%                                       %
%    Scheduling of a large batch of     %
%   independent tasks on a homogeoneous %
%                system                 %
%                                       %
%       Declarative Programming         %
%              2014-2015                %
%                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%
%%%   Cores  %%%
%%%%%%%%%%%%%%%%
% The cores of the machine
% core(Id): a core with unique identifier 'Id'.
core(c1).
core(c2).
core(c3).
core(c4).
core(c5).
core(c6).
core(c7).
core(c8).
core(c9).
core(c10).
core(c11).
core(c12).
core(c13).
core(c14).
core(c15).
core(c16).

%%%%%%%%%%%%%%%%%%
%%%   Tasks    %%%
%%%%%%%%%%%%%%%%%%
% The tasks the application is made up of, which are to be scheduled.
% task(Id): a task with unique identifier 'Id'.
task(t1).
task(t2).
task(t3).
task(t4).
task(t5).
task(t6).
task(t7).
task(t8).
task(t9).
task(t10).
task(t11).
task(t12).
task(t13).
task(t14).
task(t15).
task(t16).
task(t17).
task(t18).
task(t19).
task(t20).
task(t21).
task(t22).
task(t23).
task(t24).
task(t25).
task(t26).
task(t27).
task(t28).
task(t29).
task(t30).
task(t31).
task(t32).
task(t33).
task(t34).
task(t35).
task(t36).
task(t37).
task(t38).
task(t39).
task(t40).
task(t41).
task(t42).
task(t43).
task(t44).
task(t45).
task(t46).
task(t47).
task(t48).
task(t49).
task(t50).
task(t51).
task(t52).
task(t53).
task(t54).
task(t55).
task(t56).
task(t57).
task(t58).
task(t59).
task(t60).
task(t61).
task(t62).
task(t63).
task(t64).
task(t65).
task(t66).
task(t67).
task(t68).
task(t69).
task(t70).
task(t71).
task(t72).
task(t73).
task(t74).
task(t75).
task(t76).
task(t77).
task(t78).
task(t79).
task(t80).
task(t81).
task(t82).
task(t83).
task(t84).
task(t85).
task(t86).
task(t87).
task(t88).
task(t89).
task(t90).
task(t91).
task(t92).
task(t93).
task(t94).
task(t95).
task(t96).
task(t97).
task(t98).
task(t99).
task(t100).
task(t101).
task(t102).
task(t103).
task(t104).
task(t105).
task(t106).
task(t107).
task(t108).
task(t109).
task(t110).
task(t111).
task(t112).
task(t113).
task(t114).
task(t115).
task(t116).
task(t117).
task(t118).
task(t119).
task(t120).
task(t121).
task(t122).
task(t123).
task(t124).
task(t125).
task(t126).
task(t127).
task(t128).
task(t129).
task(t130).
task(t131).
task(t132).
task(t133).
task(t134).
task(t135).
task(t136).
task(t137).
task(t138).
task(t139).
task(t140).
task(t141).
task(t142).
task(t143).
task(t144).
task(t145).
task(t146).
task(t147).
task(t148).
task(t149).
task(t150).
task(t151).
task(t152).
task(t153).
task(t154).
task(t155).
task(t156).
task(t157).
task(t158).
task(t159).
task(t160).
task(t161).
task(t162).
task(t163).
task(t164).
task(t165).
task(t166).
task(t167).
task(t168).
task(t169).
task(t170).
task(t171).
task(t172).
task(t173).
task(t174).
task(t175).
task(t176).
task(t177).
task(t178).
task(t179).
task(t180).
task(t181).
task(t182).
task(t183).
task(t184).
task(t185).
task(t186).
task(t187).
task(t188).
task(t189).
task(t190).
task(t191).
task(t192).
task(t193).
task(t194).
task(t195).
task(t196).
task(t197).
task(t198).
task(t199).
task(t200).
task(t201).
task(t202).
task(t203).
task(t204).
task(t205).
task(t206).
task(t207).
task(t208).
task(t209).
task(t210).
task(t211).
task(t212).
task(t213).
task(t214).
task(t215).
task(t216).
task(t217).
task(t218).
task(t219).
task(t220).
task(t221).
task(t222).
task(t223).
task(t224).
task(t225).
task(t226).
task(t227).
task(t228).
task(t229).
task(t230).
task(t231).
task(t232).
task(t233).
task(t234).
task(t235).
task(t236).
task(t237).
task(t238).
task(t239).
task(t240).
task(t241).
task(t242).
task(t243).
task(t244).
task(t245).
task(t246).
task(t247).
task(t248).
task(t249).
task(t250).
task(t251).
task(t252).
task(t253).
task(t254).
task(t255).
task(t256).
task(t257).
task(t258).
task(t259).
task(t260).
task(t261).
task(t262).
task(t263).
task(t264).
task(t265).
task(t266).
task(t267).
task(t268).
task(t269).
task(t270).
task(t271).
task(t272).
task(t273).
task(t274).
task(t275).
task(t276).
task(t277).
task(t278).
task(t279).
task(t280).
task(t281).
task(t282).
task(t283).
task(t284).
task(t285).
task(t286).
task(t287).
task(t288).
task(t289).
task(t290).
task(t291).
task(t292).
task(t293).
task(t294).
task(t295).
task(t296).
task(t297).
task(t298).
task(t299).
task(t300).
task(t301).
task(t302).
task(t303).
task(t304).
task(t305).
task(t306).
task(t307).
task(t308).
task(t309).
task(t310).
task(t311).
task(t312).
task(t313).
task(t314).
task(t315).
task(t316).
task(t317).
task(t318).
task(t319).
task(t320).

%%%%%%%%%%%%%%%%%%
%% Dependencies %%
%%%%%%%%%%%%%%%%%%
% The execution order dependencies between tasks
% depends_on(Ta,Tb,Data): before task 'Ta' can be executed, 
% task 'Tb' must have been executed and thereafter 'Data' megabytes of data (result of/produced by 'Tb') must have been moved from the processor that executed 'Tb' to the processor that will execute 'Ta'.

%In this benchmark there are no dependencies between tasks.
depends_on(_,_,_) :- fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Processing Costs   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies how long the processing of each task takes on each core.
% process_cost(T,C,Time): It takes 'Time' ms to execute task 'T' on core 'C'.

%In this benchmark every core can execute a given task as fast (i.e. homogeneous system)
process_cost(t1,C,29) :- core(C).
process_cost(t2,C,34) :- core(C).
process_cost(t3,C,34) :- core(C).
process_cost(t4,C,21) :- core(C).
process_cost(t5,C,11) :- core(C).
process_cost(t6,C,94) :- core(C).
process_cost(t7,C,30) :- core(C).
process_cost(t8,C,50) :- core(C).
process_cost(t9,C,98) :- core(C).
process_cost(t10,C,38) :- core(C).
process_cost(t11,C,61) :- core(C).
process_cost(t12,C,39) :- core(C).
process_cost(t13,C,48) :- core(C).
process_cost(t14,C,87) :- core(C).
process_cost(t15,C,90) :- core(C).
process_cost(t16,C,65) :- core(C).
process_cost(t17,C,51) :- core(C).
process_cost(t18,C,13) :- core(C).
process_cost(t19,C,98) :- core(C).
process_cost(t20,C,28) :- core(C).
process_cost(t21,C,75) :- core(C).
process_cost(t22,C,60) :- core(C).
process_cost(t23,C,8) :- core(C).
process_cost(t24,C,47) :- core(C).
process_cost(t25,C,79) :- core(C).
process_cost(t26,C,64) :- core(C).
process_cost(t27,C,73) :- core(C).
process_cost(t28,C,97) :- core(C).
process_cost(t29,C,90) :- core(C).
process_cost(t30,C,7) :- core(C).
process_cost(t31,C,19) :- core(C).
process_cost(t32,C,45) :- core(C).
process_cost(t33,C,55) :- core(C).
process_cost(t34,C,27) :- core(C).
process_cost(t35,C,51) :- core(C).
process_cost(t36,C,55) :- core(C).
process_cost(t37,C,74) :- core(C).
process_cost(t38,C,19) :- core(C).
process_cost(t39,C,54) :- core(C).
process_cost(t40,C,44) :- core(C).
process_cost(t41,C,42) :- core(C).
process_cost(t42,C,91) :- core(C).
process_cost(t43,C,77) :- core(C).
process_cost(t44,C,24) :- core(C).
process_cost(t45,C,91) :- core(C).
process_cost(t46,C,68) :- core(C).
process_cost(t47,C,45) :- core(C).
process_cost(t48,C,11) :- core(C).
process_cost(t49,C,57) :- core(C).
process_cost(t50,C,29) :- core(C).
process_cost(t51,C,98) :- core(C).
process_cost(t52,C,1) :- core(C).
process_cost(t53,C,7) :- core(C).
process_cost(t54,C,97) :- core(C).
process_cost(t55,C,41) :- core(C).
process_cost(t56,C,35) :- core(C).
process_cost(t57,C,19) :- core(C).
process_cost(t58,C,54) :- core(C).
process_cost(t59,C,21) :- core(C).
process_cost(t60,C,80) :- core(C).
process_cost(t61,C,63) :- core(C).
process_cost(t62,C,44) :- core(C).
process_cost(t63,C,30) :- core(C).
process_cost(t64,C,4) :- core(C).
process_cost(t65,C,57) :- core(C).
process_cost(t66,C,31) :- core(C).
process_cost(t67,C,4) :- core(C).
process_cost(t68,C,29) :- core(C).
process_cost(t69,C,96) :- core(C).
process_cost(t70,C,21) :- core(C).
process_cost(t71,C,67) :- core(C).
process_cost(t72,C,79) :- core(C).
process_cost(t73,C,90) :- core(C).
process_cost(t74,C,60) :- core(C).
process_cost(t75,C,85) :- core(C).
process_cost(t76,C,98) :- core(C).
process_cost(t77,C,74) :- core(C).
process_cost(t78,C,69) :- core(C).
process_cost(t79,C,58) :- core(C).
process_cost(t80,C,100) :- core(C).
process_cost(t81,C,15) :- core(C).
process_cost(t82,C,74) :- core(C).
process_cost(t83,C,77) :- core(C).
process_cost(t84,C,52) :- core(C).
process_cost(t85,C,76) :- core(C).
process_cost(t86,C,94) :- core(C).
process_cost(t87,C,71) :- core(C).
process_cost(t88,C,83) :- core(C).
process_cost(t89,C,24) :- core(C).
process_cost(t90,C,79) :- core(C).
process_cost(t91,C,55) :- core(C).
process_cost(t92,C,29) :- core(C).
process_cost(t93,C,46) :- core(C).
process_cost(t94,C,77) :- core(C).
process_cost(t95,C,69) :- core(C).
process_cost(t96,C,92) :- core(C).
process_cost(t97,C,65) :- core(C).
process_cost(t98,C,62) :- core(C).
process_cost(t99,C,85) :- core(C).
process_cost(t100,C,58) :- core(C).
process_cost(t101,C,97) :- core(C).
process_cost(t102,C,2) :- core(C).
process_cost(t103,C,97) :- core(C).
process_cost(t104,C,33) :- core(C).
process_cost(t105,C,85) :- core(C).
process_cost(t106,C,81) :- core(C).
process_cost(t107,C,71) :- core(C).
process_cost(t108,C,4) :- core(C).
process_cost(t109,C,6) :- core(C).
process_cost(t110,C,7) :- core(C).
process_cost(t111,C,60) :- core(C).
process_cost(t112,C,12) :- core(C).
process_cost(t113,C,54) :- core(C).
process_cost(t114,C,57) :- core(C).
process_cost(t115,C,61) :- core(C).
process_cost(t116,C,12) :- core(C).
process_cost(t117,C,87) :- core(C).
process_cost(t118,C,11) :- core(C).
process_cost(t119,C,21) :- core(C).
process_cost(t120,C,55) :- core(C).
process_cost(t121,C,50) :- core(C).
process_cost(t122,C,70) :- core(C).
process_cost(t123,C,65) :- core(C).
process_cost(t124,C,9) :- core(C).
process_cost(t125,C,75) :- core(C).
process_cost(t126,C,7) :- core(C).
process_cost(t127,C,92) :- core(C).
process_cost(t128,C,93) :- core(C).
process_cost(t129,C,92) :- core(C).
process_cost(t130,C,69) :- core(C).
process_cost(t131,C,18) :- core(C).
process_cost(t132,C,14) :- core(C).
process_cost(t133,C,90) :- core(C).
process_cost(t134,C,8) :- core(C).
process_cost(t135,C,35) :- core(C).
process_cost(t136,C,52) :- core(C).
process_cost(t137,C,72) :- core(C).
process_cost(t138,C,47) :- core(C).
process_cost(t139,C,66) :- core(C).
process_cost(t140,C,18) :- core(C).
process_cost(t141,C,2) :- core(C).
process_cost(t142,C,5) :- core(C).
process_cost(t143,C,88) :- core(C).
process_cost(t144,C,7) :- core(C).
process_cost(t145,C,6) :- core(C).
process_cost(t146,C,20) :- core(C).
process_cost(t147,C,36) :- core(C).
process_cost(t148,C,30) :- core(C).
process_cost(t149,C,82) :- core(C).
process_cost(t150,C,71) :- core(C).
process_cost(t151,C,100) :- core(C).
process_cost(t152,C,35) :- core(C).
process_cost(t153,C,83) :- core(C).
process_cost(t154,C,3) :- core(C).
process_cost(t155,C,70) :- core(C).
process_cost(t156,C,27) :- core(C).
process_cost(t157,C,99) :- core(C).
process_cost(t158,C,45) :- core(C).
process_cost(t159,C,74) :- core(C).
process_cost(t160,C,35) :- core(C).
process_cost(t161,C,97) :- core(C).
process_cost(t162,C,18) :- core(C).
process_cost(t163,C,11) :- core(C).
process_cost(t164,C,70) :- core(C).
process_cost(t165,C,17) :- core(C).
process_cost(t166,C,51) :- core(C).
process_cost(t167,C,78) :- core(C).
process_cost(t168,C,77) :- core(C).
process_cost(t169,C,87) :- core(C).
process_cost(t170,C,24) :- core(C).
process_cost(t171,C,49) :- core(C).
process_cost(t172,C,69) :- core(C).
process_cost(t173,C,38) :- core(C).
process_cost(t174,C,57) :- core(C).
process_cost(t175,C,78) :- core(C).
process_cost(t176,C,21) :- core(C).
process_cost(t177,C,26) :- core(C).
process_cost(t178,C,1) :- core(C).
process_cost(t179,C,24) :- core(C).
process_cost(t180,C,49) :- core(C).
process_cost(t181,C,79) :- core(C).
process_cost(t182,C,19) :- core(C).
process_cost(t183,C,91) :- core(C).
process_cost(t184,C,82) :- core(C).
process_cost(t185,C,50) :- core(C).
process_cost(t186,C,62) :- core(C).
process_cost(t187,C,9) :- core(C).
process_cost(t188,C,36) :- core(C).
process_cost(t189,C,97) :- core(C).
process_cost(t190,C,8) :- core(C).
process_cost(t191,C,4) :- core(C).
process_cost(t192,C,70) :- core(C).
process_cost(t193,C,38) :- core(C).
process_cost(t194,C,75) :- core(C).
process_cost(t195,C,80) :- core(C).
process_cost(t196,C,45) :- core(C).
process_cost(t197,C,45) :- core(C).
process_cost(t198,C,29) :- core(C).
process_cost(t199,C,13) :- core(C).
process_cost(t200,C,68) :- core(C).
process_cost(t201,C,70) :- core(C).
process_cost(t202,C,38) :- core(C).
process_cost(t203,C,14) :- core(C).
process_cost(t204,C,45) :- core(C).
process_cost(t205,C,27) :- core(C).
process_cost(t206,C,48) :- core(C).
process_cost(t207,C,29) :- core(C).
process_cost(t208,C,13) :- core(C).
process_cost(t209,C,100) :- core(C).
process_cost(t210,C,15) :- core(C).
process_cost(t211,C,55) :- core(C).
process_cost(t212,C,69) :- core(C).
process_cost(t213,C,22) :- core(C).
process_cost(t214,C,67) :- core(C).
process_cost(t215,C,80) :- core(C).
process_cost(t216,C,26) :- core(C).
process_cost(t217,C,70) :- core(C).
process_cost(t218,C,23) :- core(C).
process_cost(t219,C,28) :- core(C).
process_cost(t220,C,27) :- core(C).
process_cost(t221,C,45) :- core(C).
process_cost(t222,C,52) :- core(C).
process_cost(t223,C,61) :- core(C).
process_cost(t224,C,25) :- core(C).
process_cost(t225,C,9) :- core(C).
process_cost(t226,C,87) :- core(C).
process_cost(t227,C,49) :- core(C).
process_cost(t228,C,52) :- core(C).
process_cost(t229,C,19) :- core(C).
process_cost(t230,C,84) :- core(C).
process_cost(t231,C,56) :- core(C).
process_cost(t232,C,89) :- core(C).
process_cost(t233,C,49) :- core(C).
process_cost(t234,C,96) :- core(C).
process_cost(t235,C,16) :- core(C).
process_cost(t236,C,8) :- core(C).
process_cost(t237,C,40) :- core(C).
process_cost(t238,C,70) :- core(C).
process_cost(t239,C,33) :- core(C).
process_cost(t240,C,9) :- core(C).
process_cost(t241,C,38) :- core(C).
process_cost(t242,C,38) :- core(C).
process_cost(t243,C,72) :- core(C).
process_cost(t244,C,31) :- core(C).
process_cost(t245,C,49) :- core(C).
process_cost(t246,C,10) :- core(C).
process_cost(t247,C,19) :- core(C).
process_cost(t248,C,53) :- core(C).
process_cost(t249,C,80) :- core(C).
process_cost(t250,C,51) :- core(C).
process_cost(t251,C,78) :- core(C).
process_cost(t252,C,44) :- core(C).
process_cost(t253,C,27) :- core(C).
process_cost(t254,C,96) :- core(C).
process_cost(t255,C,54) :- core(C).
process_cost(t256,C,38) :- core(C).
process_cost(t257,C,55) :- core(C).
process_cost(t258,C,78) :- core(C).
process_cost(t259,C,34) :- core(C).
process_cost(t260,C,18) :- core(C).
process_cost(t261,C,73) :- core(C).
process_cost(t262,C,4) :- core(C).
process_cost(t263,C,93) :- core(C).
process_cost(t264,C,44) :- core(C).
process_cost(t265,C,11) :- core(C).
process_cost(t266,C,86) :- core(C).
process_cost(t267,C,67) :- core(C).
process_cost(t268,C,44) :- core(C).
process_cost(t269,C,61) :- core(C).
process_cost(t270,C,94) :- core(C).
process_cost(t271,C,99) :- core(C).
process_cost(t272,C,3) :- core(C).
process_cost(t273,C,89) :- core(C).
process_cost(t274,C,66) :- core(C).
process_cost(t275,C,22) :- core(C).
process_cost(t276,C,35) :- core(C).
process_cost(t277,C,58) :- core(C).
process_cost(t278,C,85) :- core(C).
process_cost(t279,C,5) :- core(C).
process_cost(t280,C,97) :- core(C).
process_cost(t281,C,19) :- core(C).
process_cost(t282,C,36) :- core(C).
process_cost(t283,C,27) :- core(C).
process_cost(t284,C,4) :- core(C).
process_cost(t285,C,72) :- core(C).
process_cost(t286,C,25) :- core(C).
process_cost(t287,C,94) :- core(C).
process_cost(t288,C,51) :- core(C).
process_cost(t289,C,64) :- core(C).
process_cost(t290,C,92) :- core(C).
process_cost(t291,C,44) :- core(C).
process_cost(t292,C,60) :- core(C).
process_cost(t293,C,70) :- core(C).
process_cost(t294,C,71) :- core(C).
process_cost(t295,C,85) :- core(C).
process_cost(t296,C,81) :- core(C).
process_cost(t297,C,13) :- core(C).
process_cost(t298,C,28) :- core(C).
process_cost(t299,C,2) :- core(C).
process_cost(t300,C,21) :- core(C).
process_cost(t301,C,6) :- core(C).
process_cost(t302,C,81) :- core(C).
process_cost(t303,C,58) :- core(C).
process_cost(t304,C,85) :- core(C).
process_cost(t305,C,65) :- core(C).
process_cost(t306,C,43) :- core(C).
process_cost(t307,C,3) :- core(C).
process_cost(t308,C,31) :- core(C).
process_cost(t309,C,4) :- core(C).
process_cost(t310,C,56) :- core(C).
process_cost(t311,C,22) :- core(C).
process_cost(t312,C,64) :- core(C).
process_cost(t313,C,9) :- core(C).
process_cost(t314,C,92) :- core(C).
process_cost(t315,C,7) :- core(C).
process_cost(t316,C,82) :- core(C).
process_cost(t317,C,67) :- core(C).
process_cost(t318,C,95) :- core(C).
process_cost(t319,C,34) :- core(C).
process_cost(t320,C,18) :- core(C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Channel Properties  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Specifies the properties of the communication channel between each of the cores
% channel(Ca,Cb,Latency,Bandwidth): The channel to communicate from core 'Ca' to core 'Cb' has a latency 'Latency' and bandwidth 'Bandwidth'.
% Note that sending 'X' megabytes of data, over a channel, takes Latency + X/Bandwidth ms.

%In this benchmark (without task dependencies), no inter core communication is required, the channel properties therefore do not matter.
channel(C1,C2,0,1) :- core(C1), core(C2).

