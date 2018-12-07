context("Active Risks")

library(tidymas)

unwt_return <- read.table(text = "    date       strategy                        return
                          1 2018-11-16 Fltnr_US_ILB_5s10s:::jule -0.000753
                          2 2018-11-19 Fltnr_US_ILB_5s10s:::jule -0.000413
                          3 2018-11-20 Fltnr_US_ILB_5s10s:::jule  0.000332
                          4 2018-11-21 Fltnr_US_ILB_5s10s:::jule  0.000402
                          5 2018-11-22 Fltnr_US_ILB_5s10s:::jule  0
                          6 2018-11-23 Fltnr_US_ILB_5s10s:::jule  0.000647
                          7 2018-11-26 Fltnr_US_ILB_5s10s:::jule -0.000197
                          8 2018-11-27 Fltnr_US_ILB_5s10s:::jule  0.000183
                          9 2018-11-28 Fltnr_US_ILB_5s10s:::jule -0.000122
                          10 2018-11-29 Fltnr_US_ILB_5s10s:::jule -0.0000894
                          11 2018-11-30 Fltnr_US_ILB_5s10s:::jule  0.000399
                          12 2018-12-03 Fltnr_US_ILB_5s10s:::jule  0.000413
                          13 2018-12-04 Fltnr_US_ILB_5s10s:::jule  0.000883
                          14 2018-12-05 Fltnr_US_ILB_5s10s:::jule  0
                          15 2018-12-06 Fltnr_US_ILB_5s10s:::jule  0.000419
                          16 2018-12-07 Fltnr_US_ILB_5s10s:::jule  0
                          17 2018-11-16 L_ES_S_BE_10y:::eve        0.000535
                          18 2018-11-19 L_ES_S_BE_10y:::eve        0.000793
                          19 2018-11-20 L_ES_S_BE_10y:::eve       -0.00126
                          20 2018-11-21 L_ES_S_BE_10y:::eve        0.00160
                          21 2018-11-22 L_ES_S_BE_10y:::eve       -0.0000857
                          22 2018-11-23 L_ES_S_BE_10y:::eve       -0.00163
                          23 2018-11-26 L_ES_S_BE_10y:::eve        0.00694
                          24 2018-11-27 L_ES_S_BE_10y:::eve       -0.000511
                          25 2018-11-28 L_ES_S_BE_10y:::eve        0.000877
                          26 2018-11-29 L_ES_S_BE_10y:::eve        0.000379
                          27 2018-11-30 L_ES_S_BE_10y:::eve       -0.000144
                          28 2018-12-03 L_ES_S_BE_10y:::eve        0.00161
                          29 2018-12-04 L_ES_S_BE_10y:::eve       -0.00154
                          30 2018-12-05 L_ES_S_BE_10y:::eve        0.00248
                          31 2018-12-06 L_ES_S_BE_10y:::eve       -0.00223
                          32 2018-12-07 L_ES_S_BE_10y:::eve        0
                          33 2018-11-16 L_EUR:::kev                0.00658
                          34 2018-11-19 L_EUR:::kev                0.00412
                          35 2018-11-20 L_EUR:::kev               -0.00254
                          36 2018-11-21 L_EUR:::kev               -0.00160
                          37 2018-11-22 L_EUR:::kev                0.000104
                          38 2018-11-23 L_EUR:::kev               -0.00405
                          39 2018-11-26 L_EUR:::kev               -0.000353
                          40 2018-11-27 L_EUR:::kev               -0.00216
                          41 2018-11-28 L_EUR:::kev                0.00610
                          42 2018-11-29 L_EUR:::kev                0.00256
                          43 2018-11-30 L_EUR:::kev               -0.00636
                          44 2018-12-03 L_EUR:::kev               -0.000333
                          45 2018-12-04 L_EUR:::kev                0.00123
                          46 2018-12-05 L_EUR:::kev                0.00347
                          47 2018-12-06 L_EUR:::kev                0.00356
                          48 2018-12-07 L_EUR:::kev                0.000426
                          49 2018-11-16 L_Nikkei_eg_chgsize:::ben -0.00565
                          50 2018-11-19 L_Nikkei_eg_chgsize:::ben  0.00650
                          51 2018-11-20 L_Nikkei_eg_chgsize:::ben -0.0109
                          52 2018-11-21 L_Nikkei_eg_chgsize:::ben -0.00350
                          53 2018-11-22 L_Nikkei_eg_chgsize:::ben  0.00646
                          54 2018-11-23 L_Nikkei_eg_chgsize:::ben  0
                          55 2018-11-26 L_Nikkei_eg_chgsize:::ben  0.00764
                          56 2018-11-27 L_Nikkei_eg_chgsize:::ben  0.00644
                          57 2018-11-28 L_Nikkei_eg_chgsize:::ben  0.0102
                          58 2018-11-29 L_Nikkei_eg_chgsize:::ben  0.00386
                          59 2018-11-30 L_Nikkei_eg_chgsize:::ben  0.00397
                          60 2018-12-03 L_Nikkei_eg_chgsize:::ben  0.0100
                          61 2018-12-04 L_Nikkei_eg_chgsize:::ben -0.0239
                          62 2018-12-05 L_Nikkei_eg_chgsize:::ben -0.00530
                          63 2018-12-06 L_Nikkei_eg_chgsize:::ben -0.0191
                          64 2018-12-07 L_Nikkei_eg_chgsize:::ben  0.00104
                          65 2018-11-16 L_STOXX_S_FTSE:::eve       0.000429
                          66 2018-11-19 L_STOXX_S_FTSE:::eve      -0.00456
                          67 2018-11-20 L_STOXX_S_FTSE:::eve      -0.00644
                          68 2018-11-21 L_STOXX_S_FTSE:::eve      -0.00258
                          69 2018-11-22 L_STOXX_S_FTSE:::eve       0.00412
                          70 2018-11-23 L_STOXX_S_FTSE:::eve       0.00444
                          71 2018-11-26 L_STOXX_S_FTSE:::eve      -0.000642
                          72 2018-11-27 L_STOXX_S_FTSE:::eve       0.000739
                          73 2018-11-28 L_STOXX_S_FTSE:::eve       0.00235
                          74 2018-11-29 L_STOXX_S_FTSE:::eve      -0.00306
                          75 2018-11-30 L_STOXX_S_FTSE:::eve       0.00802
                          76 2018-12-03 L_STOXX_S_FTSE:::eve       0.00142
                          77 2018-12-04 L_STOXX_S_FTSE:::eve      -0.00239
                          78 2018-12-05 L_STOXX_S_FTSE:::eve       0.00215
                          79 2018-12-06 L_STOXX_S_FTSE:::eve      -0.00165
                          80 2018-12-07 L_STOXX_S_FTSE:::eve       0
                          81 2018-11-16 L_US_BreakE_10y:::jule     0
                          82 2018-11-19 L_US_BreakE_10y:::jule     0
                          83 2018-11-20 L_US_BreakE_10y:::jule     0
                          84 2018-11-21 L_US_BreakE_10y:::jule     0
                          85 2018-11-22 L_US_BreakE_10y:::jule     0
                          86 2018-11-23 L_US_BreakE_10y:::jule     0
                          87 2018-11-26 L_US_BreakE_10y:::jule     0
                          88 2018-11-27 L_US_BreakE_10y:::jule     0
                          89 2018-11-28 L_US_BreakE_10y:::jule     0
                          90 2018-11-29 L_US_BreakE_10y:::jule     0
                          91 2018-11-30 L_US_BreakE_10y:::jule     0
                          92 2018-12-03 L_US_BreakE_10y:::jule     0
                          93 2018-12-04 L_US_BreakE_10y:::jule     0
                          94 2018-12-05 L_US_BreakE_10y:::jule     0
                          95 2018-12-06 L_US_BreakE_10y:::jule     0
                          96 2018-12-07 L_US_BreakE_10y:::jule     0
                          97 2018-11-16 S_DE_10y_fut:::eve         0
                          98 2018-11-19 S_DE_10y_fut:::eve         0
                          99 2018-11-20 S_DE_10y_fut:::eve         0
                          100 2018-11-21 S_DE_10y_fut:::eve         0
                          101 2018-11-22 S_DE_10y_fut:::eve         0
                          102 2018-11-23 S_DE_10y_fut:::eve         0
                          103 2018-11-26 S_DE_10y_fut:::eve         0
                          104 2018-11-27 S_DE_10y_fut:::eve         0
                          105 2018-11-28 S_DE_10y_fut:::eve         0
                          106 2018-11-29 S_DE_10y_fut:::eve         0
                          107 2018-11-30 S_DE_10y_fut:::eve         0
                          108 2018-12-03 S_DE_10y_fut:::eve         0
                          109 2018-12-04 S_DE_10y_fut:::eve         0
                          110 2018-12-05 S_DE_10y_fut:::eve         0
                          111 2018-12-06 S_DE_10y_fut:::eve         0
                          112 2018-12-07 S_DE_10y_fut:::eve         0
                          113 2018-11-16 S_US_IG_CDS:::ben          0.000428
                          114 2018-11-19 S_US_IG_CDS:::ben          0.000980
                          115 2018-11-20 S_US_IG_CDS:::ben          0.000913
                          116 2018-11-21 S_US_IG_CDS:::ben         -0.000545
                          117 2018-11-22 S_US_IG_CDS:::ben          0
                          118 2018-11-23 S_US_IG_CDS:::ben          0.000771
                          119 2018-11-26 S_US_IG_CDS:::ben         -0.000832
                          120 2018-11-27 S_US_IG_CDS:::ben         -0.000299
                          121 2018-11-28 S_US_IG_CDS:::ben         -0.00234
                          122 2018-11-29 S_US_IG_CDS:::ben          0.000335
                          123 2018-11-30 S_US_IG_CDS:::ben          0.000198
                          124 2018-12-03 S_US_IG_CDS:::ben         -0.000569
                          125 2018-12-04 S_US_IG_CDS:::ben          0.00210
                          126 2018-12-05 S_US_IG_CDS:::ben         -0.0000954
                          127 2018-12-06 S_US_IG_CDS:::ben         -0.0000639
                          128 2018-12-07 S_US_IG_CDS:::ben          0
                          129 2018-11-16 S_US_ILB:::ben            -0.00509
                          130 2018-11-19 S_US_ILB:::ben             0.000618
                          131 2018-11-20 S_US_ILB:::ben             0.000692
                          132 2018-11-21 S_US_ILB:::ben             0.000870
                          133 2018-11-22 S_US_ILB:::ben             0
                          134 2018-11-23 S_US_ILB:::ben             0.000811
                          135 2018-11-26 S_US_ILB:::ben             0.00128
                          136 2018-11-27 S_US_ILB:::ben             0.00237
                          137 2018-11-28 S_US_ILB:::ben            -0.00292
                          138 2018-11-29 S_US_ILB:::ben            -0.00387
                          139 2018-11-30 S_US_ILB:::ben            -0.00201
                          140 2018-12-03 S_US_ILB:::ben            -0.000770
                          141 2018-12-04 S_US_ILB:::ben            -0.00350
                          142 2018-12-05 S_US_ILB:::ben             0
                          143 2018-12-06 S_US_ILB:::ben            -0.000757
                          144 2018-12-07 S_US_ILB:::ben             0
                          145 2018-11-16 Stpnr_Euribor_3-7:::eve    0.000000135
                          146 2018-11-19 Stpnr_Euribor_3-7:::eve   -0.000000135
                          147 2018-11-20 Stpnr_Euribor_3-7:::eve   -0.0000501
                          148 2018-11-21 Stpnr_Euribor_3-7:::eve    0
                          149 2018-11-22 Stpnr_Euribor_3-7:::eve   -0.0000500
                          150 2018-11-23 Stpnr_Euribor_3-7:::eve   -0.0001000
                          151 2018-11-26 Stpnr_Euribor_3-7:::eve    0.0000500
                          152 2018-11-27 Stpnr_Euribor_3-7:::eve   -0.0000501
                          153 2018-11-28 Stpnr_Euribor_3-7:::eve    0
                          154 2018-11-29 Stpnr_Euribor_3-7:::eve   -0.000200
                          155 2018-11-30 Stpnr_Euribor_3-7:::eve   -0.0000999
                          156 2018-12-03 Stpnr_Euribor_3-7:::eve    0
                          157 2018-12-04 Stpnr_Euribor_3-7:::eve   -0.000200
                          158 2018-12-05 Stpnr_Euribor_3-7:::eve    0.000150
                          159 2018-12-06 Stpnr_Euribor_3-7:::eve   -0.000150
                          160 2018-12-07 Stpnr_Euribor_3-7:::eve    0.0000501
                          161 2018-11-16 Stpnr_JP_10s30s:::ben      0.000157
                          162 2018-11-19 Stpnr_JP_10s30s:::ben     -0.0000724
                          163 2018-11-20 Stpnr_JP_10s30s:::ben     -0.000232
                          164 2018-11-21 Stpnr_JP_10s30s:::ben     -0.000377
                          165 2018-11-22 Stpnr_JP_10s30s:::ben     -0.000301
                          166 2018-11-23 Stpnr_JP_10s30s:::ben      0.000000868
                          167 2018-11-26 Stpnr_JP_10s30s:::ben     -0.0000779
                          168 2018-11-27 Stpnr_JP_10s30s:::ben     -0.000277
                          169 2018-11-28 Stpnr_JP_10s30s:::ben     -0.0000129
                          170 2018-11-29 Stpnr_JP_10s30s:::ben      0.000371
                          171 2018-11-30 Stpnr_JP_10s30s:::ben     -0.000385
                          172 2018-12-03 Stpnr_JP_10s30s:::ben      0.000328
                          173 2018-12-04 Stpnr_JP_10s30s:::ben     -0.000769
                          174 2018-12-05 Stpnr_JP_10s30s:::ben      0.000336
                          175 2018-12-06 Stpnr_JP_10s30s:::ben      0.00112
                          176 2018-12-07 Stpnr_JP_10s30s:::ben      0   ", stringsAsFactors = FALSE)

curr_wt <- read.table(text = "  date       strategy                     size
1 2018-01-05 Fltnr_US_ILB_5s10s:::jule 0.0111
2 2018-01-05 L_ES_S_BE_10y:::eve       0.00992
3 2018-01-05 L_EUR:::kev               0.0241
4 2018-01-05 L_Nikkei_eg_chgsize:::ben 0.0023
5 2018-01-05 L_STOXX_S_FTSE:::eve      0.005
6 2018-01-05 S_US_IG_CDS:::ben         0.0177
7 2018-01-05 S_US_ILB:::ben            0.00575
8 2018-01-05 Stpnr_Euribor_3-7:::eve   0.0767
9 2018-01-05 Stpnr_JP_10s30s:::ben     0.00383", stringsAsFactors = FALSE)

# I think is correct
correct_results <- read.table(text = "                   strategy   correct_ar
1 Fltnr_US_ILB_5s10s:::jule -4.616408e-05
2       L_ES_S_BE_10y:::eve  1.848666e-04
3               L_EUR:::kev  1.116318e-03
4 L_Nikkei_eg_chgsize:::ben  1.471873e-04
5      L_STOXX_S_FTSE:::eve -2.185902e-05
6         S_US_IG_CDS:::ben -1.344843e-04
7            S_US_ILB:::ben -1.439085e-05
8   Stpnr_Euribor_3-7:::eve  6.078667e-05
9     Stpnr_JP_10s30s:::ben  9.748790e-06")

test_that("calc_active_risk runs properly, but gives warning as inputs less than 20",
          expect_warning(calc_active_risk(unwt_return, curr_wt, as.Date("2018-11-17"), as.Date("2018-12-06")))
)

test_that("calc_active_risk gives error when periods less than 5",
          expect_error(calc_active_risk(unwt_return, curr_wt, as.Date("2018-12-02"), as.Date("2018-12-06"))))

test_that("calc_active_risk gives expected numbers",
          expect_true(all(
            calc_active_risk(unwt_return, curr_wt, as.Date("2018-11-17"), as.Date("2018-12-06")) %>%
              left_join(correct_results, by = "strategy") %>%
              mutate(diff = abs(.data$correct_ar - .data$active_risk)) %>%
              .$diff < 1e-9)
            )
)
