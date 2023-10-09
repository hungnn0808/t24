*-----------------------------------------------------------------------------
* <Rating>2162</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE RTN.THAY.DOI.LS.LD
*-----------------------------------------------------------------------------
* CREATED BY    : HOANGND
* UPDATED BY    : HIEUDT1 - (HIEU BOY)
* UPDATED DATE  : 24/02/2022
* DESCRIPTION   : THAY DOI LAI SUAT LD KHACH HANG CA NHAN
*-----------------------------------------------------------------------------
* Modifications:
* 05/10/2022 - T2420221005 - HUNGNN1
*              CAP NHAT LAI SUAT CO SO KHI KHONG CO DL TRONG PI
*
* 30/11/2022 - T2420221130 - HUNGNN1
*              CAP NHAT T24.LOAN.CODE
*
* 07/03/2023 - T2420230307 - HUNGNN1
*              CAP NHAT T24.LOAN.CODE
*
* 02/08/2023 - T2420230802 - HUNGNN1 - CR BY HUONGNT
*              CAP NHAT LS TU DONG
*-----------------------------------------------------------------------------
    $INCLUDE ../T24_BP I_COMMON
    $INCLUDE ../T24_BP I_EQUATE
    $INCLUDE ../T24_BP I_ENQUIRY.COMMON
    $INCLUDE ../T24_BP I_F.LD.LOANS.AND.DEPOSITS
    $INCLUDE ../T24_BP I_F.CUSTOMER
    $INCLUDE ../T24_BP I_F.DATES
    $INCLUDE ../T24_BP I_F.COLLATERAL
    $INCLUDE ../T24_BP I_F.PERIODIC.INTEREST
    $INCLUDE BP I_F.ABB.LD.INT.AUTO
    $INCLUDE BP I_F.ABB.LD.LOAI.UD.KHCN
*-----------------------------------------------------------------------------

*-- LOGON VOI USER INPUT
    EXECUTE "AUTOINT"

    FN.LD.LOAI.UD = "F.ABB.LD.LOAI.UD.KHCN"
    FV.LD.LOAI.UD = ""
    CALL OPF(FN.LD.LOAI.UD,FV.LD.LOAI.UD)

    FN.COLL = "F.COLLATERAL"
    FV.COLL = ""
    CALL OPF(FN.COLL,FV.COLL)

    FN.LD = "F.LD.LOANS.AND.DEPOSITS"
    FV.LD = ""
    CALL OPF(FN.LD,FV.LD)

    FN.CUS = "F.CUSTOMER"
    FV.CUS = ""
    CALL OPF(FN.CUS,FV.CUS)

    FN.LD.INT.AUTO = "F.ABB.LD.INT.AUTO"
    FV.LD.INT.AUTO = ""
    CALL OPF(FN.LD.INT.AUTO,FV.LD.INT.AUTO)

    FN.PI = "F.PERIODIC.INTEREST"
    FV.PI = ""
    CALL OPF(FN.PI,FV.PI)

    DEFFUN ABB.SEND.MAIL(TO.MAIL,SUBJECT.MAIL,MESSAGE.MAIL)

    GOSUB GET.REF
    Y.PATH = "MIG.DIR/AC.LD.UPD.LS"

    Y.LIST.LD2 = ""
    Y.LIST.LD1 = ""
    Y.DATE = TODAY

    OPEN "MIG.DIR" TO MIG.DIR THEN END

*-- CRT "INPUT LD ID?"
*-- INPUT LIST.LD
    LIST.LD = "ALL"

    Y.TIME = ""
    Y.TIME<-1> = "START ":OCONV(TIME(),"MTS")

    OPEN Y.PATH TO Y.PATH1 THEN END
    Y.OFS.LOG = ""

    IF (UPCASE(LIST.LD) EQ "ALL") THEN
        SEL.LD = "SELECT ":FN.LD:" WITH CATEGORY GE 21050 AND CATEGORY LE 21074 "
        SEL.LD := "AND AMOUNT GT 0"
        CALL EB.READLIST(SEL.LD,LIST.LD,"","","")
    END ELSE
        CHANGE " " TO @FM IN LIST.LD
    END

* S-T2420230802
	Y.T24.LC.LIST.TO.SKIP = 'youSTAFF-KHONG TSBD':@FM:'youSTAFFplus':@FM:'youSTAFF-TSBD':@FM:'youSTAFF-ABB Care (TSBD)':@FM:'youSTAFF-ABB Care (Khong TSBD)':@FM:'youSTAFF-KHONG TSBD-NGUOITHAN'
	Y.T24.LC.LIST.TO.SKIP := @FM:'youSTAFF-TSBD-NGUOITHAN':@FM:'youSTAFF-ABB Care (TSBD)-NGUOITHAN':@FM:'youSTAFF-ABBCare(Ko TSBD)-NGUOITHAN':@FM:'youSTAFF-KHONG TSBD-NGUOI THAN'
	Y.T24.LC.LIST.TO.SKIP := @FM:'youSTAFF-TSBD-NGUOI THAN':@FM:'CBNV-BDS.ND':@FM:'CBNV-BDS.NDA':@FM:'CBNV-BDS.XAYSUA':@FM:'CBNV-OTO.TD.MOI':@FM:'CBNV-OTO.TD.CU':@FM:'CBNV-TSBD-TD.QTS TU HDLD'
	Y.T24.LC.LIST.TO.SKIP := @FM:'CBNV-TSBD-TD.TS KHAC':@FM:'CBNV-KHONG TSBD-TD':@FM:'CBNV-BDS.ND.NGUOI THAN':@FM:'CBNV-BDS.NDA.NGUOI THAN':@FM:'CBNV-BDS.XAYSUA.NGUOI THAN'
	Y.T24.LC.LIST.TO.SKIP := @FM:'CBNV-OTO.TD.MOI.NGUOI THAN':@FM:'CBNV-OTO.TD.CU.NGUOI THAN':@FM:'CBNV-TSBD-TD.NGUOI THAN':@FM:'CBNV-KHONG TSBD-TD.NGUOI THAN':@FM:'CBNV-TSBD-TD.QTS TU HDLD'
* E-T2420230802

    YC = DCOUNT(LIST.LD,@FM)
    FOR I = 1 TO YC
        CRT @(8,10):I:"/":YC
        Y.LD.ID = LIST.LD<I>
*------ CRT Y.LD.ID
        CALL F.READ(FN.LD,Y.LD.ID,R.LD,FV.LD,"")
        Y.FLOATING.INT = R.LD<LD.LOCAL.REF,INT.POS>
        Y.NGAY.THAY.LS = R.LD<LD.LOCAL.REF,20>
        Y.START.DATE = R.LD<LD.VALUE.DATE>
*------ SOURCE.OF.FUNDS
        IF (R.LD<LD.LOCAL.REF,3> NE "") THEN
            CONTINUE
        END

        Y.PHAN.QUYEN.LS = R.LD<LD.LOCAL.REF,POS3>

*------ IF (LEFT(Y.NGAY.THAY.LS,8) GT TODAY OR Y.NGAY.THAY.LS EQ "") THEN CONTINUE  ;* 28/04/2017

        Y.CUS.ID = R.LD<LD.CUSTOMER.ID>
        CALL F.READ(FN.CUS,Y.CUS.ID,R.CUS,FV.CUS,"")
        Y.CUS.SECTOR = R.CUS<EB.CUS.SECTOR>
        IF (Y.CUS.SECTOR LT 1001) OR (Y.CUS.SECTOR GT 1999) OR (Y.CUS.SECTOR EQ 1014) OR (Y.CUS.SECTOR EQ 1015) OR (Y.CUS.SECTOR EQ 1016) THEN
            CONTINUE
        END

        Y.LIMIT.REF = R.LD<LD.LIMIT.REFERENCE>
        Y.CUOI.LSUD = R.LD<LD.LOCAL.REF,POS1>
        Y.LOAN.CODE = R.LD<LD.LOCAL.REF,POS2>
        Y.CO.CODE = R.LD<LD.CO.CODE>
        Y.LD.INT = R.LD<LD.INTEREST.RATE>
*------ LAY TAN SUAT LD MOI
        Y.LD.TERM = R.LD<LD.LOCAL.REF,POS5>
        Y.LD.TERM = Y.LD.TERM["M",1,1]
        Y.LD.NGAY.LS = R.LD<LD.LOCAL.REF,20>
        Y.LD.NGAY.LS1 = LEFT(R.LD<LD.LOCAL.REF,20>,8)
        Y.DAY = RIGHT(Y.LD.NGAY.LS,2)
        Y.LD.BIENDO.KYKE = R.LD<LD.LOCAL.REF,POS7>
        Y.COMI = ""
        Y.LOAI.UD.ID = R.LD<LD.LOCAL.REF,POS6>
        Y.UPD.CHECK = ""
        Y.UPD = 1
*S-T2420221130
        Y.ADDITIONAL.INFO = R.LD<LD.LOCAL.REF,POS8>
        GOSUB SUB.UPDATE.T24.LOAN.CODE
*E-T2420221130

        IF (Y.LD.TERM GE 3) THEN
            Y.LD.NGAY.LS = LEFT(Y.LD.NGAY.LS,6):Y.DAY
            IF (Y.LD.NGAY.LS = "") OR (Y.LOAI.UD.ID NE "") THEN
                Y.LD.NGAY.LS = Y.START.DATE
                Y.DAY = RIGHT(Y.START.DATE,2)
            END
            COMI = Y.LD.NGAY.LS:"M03":Y.DAY
            CALL CFQ
        END
        ELSE
            IF (LEN(Y.LD.TERM) LT 2) THEN
                Y.LD.TERM = "0":Y.LD.TERM
            END
            IF (Y.LD.NGAY.LS = "") OR (Y.LOAI.UD.ID NE "") THEN
                Y.LD.NGAY.LS = Y.START.DATE
                Y.DAY = RIGHT(Y.START.DATE,2)
            END
            COMI = LEFT(Y.LD.NGAY.LS,6):Y.DAY:"M":Y.LD.TERM:Y.DAY
            CALL CFQ
        END
        Y.COMI = COMI
        FOR I3 = 1 TO 1000
            IF (LEFT(Y.COMI,8) LE TODAY) THEN
                COMI = Y.COMI
                CALL CFQ
                Y.COMI = COMI
                IF (LEFT(Y.COMI,8) GT TODAY) THEN
                    BREAK
                END
            END ELSE
                BREAK
            END
        NEXT I3

        Y.LD.NGAY.LS = Y.COMI

        IF (Y.LOAI.UD.ID EQ "") THEN
            Y.CUOI.LSUD = R.LD<LD.LOCAL.REF,POS1>
            IF (Y.CUOI.LSUD NE "") THEN
                CALL CDT(84,Y.CUOI.LSUD,1)
            END
            IF (LEN(Y.CUOI.LSUD) GT 8) THEN
                CONTINUE
            END
            IF (Y.CUOI.LSUD NE "" AND Y.CUOI.LSUD GT TODAY) OR (Y.CUOI.LSUD EQ "" AND Y.LD.NGAY.LS1 GT TODAY) THEN
                GOSUB UPD.NGAY.LS.BD
                CONTINUE
            END
        END

        IF (Y.PHAN.QUYEN.LS EQ "TRUONG HOP KHAC") THEN
            CONTINUE          ;* 28/04
        END

        IF (Y.LOAI.UD.ID EQ "") AND (Y.PHAN.QUYEN.LS EQ "DUOC PHAN QUYEN LAI SUAT") AND (Y.CUOI.LSUD GT TODAY) THEN
            GOSUB UPD.NGAY.LS.BD
            CONTINUE
        END
        IF (Y.PHAN.QUYEN.LS EQ "DUOC PHAN QUYEN LAI SUAT" AND Y.CUOI.LSUD LE TODAY AND Y.LOAI.UD.ID EQ "") OR (Y.PHAN.QUYEN.LS EQ "" AND Y.LOAI.UD.ID EQ "") THEN
            GOSUB CHECK.LS.TT
            CONTINUE
        END
        IF (Y.LOAI.UD.ID NE "") THEN
            GOSUB CHECK.LS.CTKM.BD
            CONTINUE
        END
    NEXT I

*-- CHAY OFS UPDATE & WRITE LOG
    WRITE "BAT DAU LOG ":OCONV(TIME(),"MTS") ON Y.PATH1,"LD.UPD.LS.KHCN.":TODAY:".TXT" ON ERROR END
    IF (Y.OFS.LOG NE "") THEN
        YC = DCOUNT(Y.OFS.LOG,@FM)
        FOR I = 1 TO YC
            CRT @(8,11):"dang ghi ofs ":I:"/":YC
            OFS.MSG = Y.OFS.LOG<I>
            GOSUB RUN.OFS
        NEXT I
    END
    READ Y.LOG FROM Y.PATH1,"LD.UPD.LS.KHCN.":TODAY:".TXT" THEN
        Y.LOG<-1> = "KET THUC LOG ":OCONV(TIME(),"MTS")
        WRITE Y.LOG ON Y.PATH1,"LD.UPD.LS.KHCN.":TODAY:".TXT" ON ERROR END
    END

    CRT "Send mail TDLS"
    GOSUB LD.SEND.MAIL
    READ Y.LOG FROM Y.PATH1,"LD.KHCN.MAIL.SENT.LIST.":Y.DATE:".TXT" THEN
        Y.LOG<-1> = "*** KET THUC GUI MAIL ***":OCONV(TIME(),"MTS")
        WRITE Y.LOG ON Y.PATH1,"LD.KHCN.MAIL.SENT.LIST.":Y.DATE:".TXT" ON ERROR END
    END

    RETURN

*-----------------------------------------------------------------------------
CHECK.LS.TT:
*-----------
*-- CHECK LAI SUAT CHO VAY THONG THUONG THEO BIEN DO -> DATE: 25/02/2019
    IF (LEN(Y.CUOI.LSUD) GT 8) THEN
        RETURN
    END
    IF (Y.CUOI.LSUD NE "") AND (Y.CUOI.LSUD LT TODAY) AND (Y.LD.NGAY.LS1 GT TODAY) THEN
        RETURN
    END

    IF (COUNT("26020.01;12020.01;26040.01;12040.01",Y.LIMIT.REF) GT 0) THEN
        GOSUB GET.LS.TC
        Y.LS = Y.LS1
        GOSUB GET.LSCS.PI
*------ MAX BIEN DO KY KE VA BIEU LS KHCN TIN CHAP
        IF (Y.LD.BIENDO.KYKE NE "") THEN
            Y.LS = MAXIMUM(Y.LS1:@AM:Y.LD.BIENDO.KYKE)
            Y.UPD = 2
        END
        GOSUB UPD.LS.LD.BD
    END ELSE
        IF (COUNT("26010.01;12010.01;26030.01;12030.01",Y.LIMIT.REF) GT 0) THEN
            GOSUB GET.LS.TC
            Y.LS = Y.LS2
            GOSUB GET.LSCS.PI
*---------- MAX BIEN DO KY KE VA BIEU LS KHCN THE CHAP
            IF (Y.LD.BIENDO.KYKE NE "") THEN
                Y.LS = MAXIMUM(Y.LS2:@AM:Y.LD.BIENDO.KYKE)
                Y.UPD = 2
            END
            GOSUB UPD.LS.LD.BD
        END ELSE
*---------- XEL COLLATERAL ID
            Y.COLL.ID = R.LD<LD.LOCAL.REF,POS4>
            GOSUB GET.LS.TC
            IF (Y.COLL.ID EQ "") THEN
                Y.LS = Y.LS1
                GOSUB GET.LSCS.PI
*-------------- MAX BIEN DO KY KE VA BIEU LS KHCN TIN CHAP
                IF (Y.LD.BIENDO.KYKE NE "") THEN
                    Y.LS = MAXIMUM(Y.LS1:@AM:Y.LD.BIENDO.KYKE)
                    Y.UPD = 2
                END
                GOSUB UPD.LS.LD.BD
            END ELSE
                Y.LS = Y.LS2
                GOSUB GET.LSCS.PI
*-------------- MAX BIEN DO KY KE VA BIEU LS KHCN THE CHAP
                IF (Y.LD.BIENDO.KYKE NE "") THEN
                    Y.LS = MAXIMUM(Y.LS2:@AM:Y.LD.BIENDO.KYKE)
                    Y.UPD = 2
                END
                GOSUB UPD.LS.LD.BD
            END
        END
    END

    RETURN

*-----------------------------------------------------------------------------
CHECK.LS.CTKM.BD:
*----------------
*-- CHECK LAI SUAT BIEN DO CHO VAY THAM GIA CTKM -> DATE: 25/02/2019
    IF (Y.LOAI.UD.ID NE "") THEN
        Y.DAY = RIGHT(Y.START.DATE,2)
        COMI = Y.START.DATE:"M03":Y.DAY

        FOR I4 = 1 TO 1000
            IF (LEFT(COMI,8) LT TODAY) THEN
                CALL CFQ
                IF (LEFT(COMI,8) GE TODAY) THEN
                    BREAK
                END
            END ELSE
                BREAK
            END
        NEXT I4
        Y.NGAY.THAY.LS = COMI

*-----------------------------------------------------------------------------
*------ CHO NAY DE DANH BO SUNG PHAN BO QUA CAC LD NGOAI LE CTKM CO NGAY CUOI LSUD > TODAY
        Y.CUOI.LSUD = R.LD<LD.LOCAL.REF,POS1>
        IF (Y.CUOI.LSUD NE "") THEN
            CALL CDT(84,Y.CUOI.LSUD,1)
        END
        IF (LEN(Y.CUOI.LSUD) GT 8) THEN
            RETURN
        END
        IF (Y.CUOI.LSUD NE "") AND (Y.CUOI.LSUD GT TODAY) THEN
            GOSUB UPD.NGAY.LS.BD
            RETURN
        END
*-----------------------------------------------------------------------------

        Y.LOAI.UD.ID = R.LD<LD.LOCAL.REF,POS6>
        CALL F.READ(FN.LD.LOAI.UD,Y.LOAI.UD.ID,R.LD.LOAIUD,FV.LD.LOAI.UD,"")
        Y.LOAIUD.PRODUCT = R.LD.LOAIUD<LD.LOAIUD.PRODUCT>
        Y.LSCS = R.LD.LOAIUD<LD.LOAIUD.LOCAL.REF,POS41>

        GOSUB GET.LSCS.PI

        YC1 = DCOUNT(R.LD<LD.LOCAL.REF,29>,SM)
        YCHECK = 0
        FOR J1 = 1 TO YC1
            Y.ADD.INFO = R.LD<LD.LOCAL.REF,29,J1>
            IF (Y.ADD.INFO EQ Y.LOAIUD.PRODUCT) THEN
                YCHECK = 1
                BREAK
            END
        NEXT J1
        IF (YCHECK EQ 0) THEN
            RETURN
        END

*------ XET T24.LOAN.CODE
        Y.VAL.DATE = R.LD<LD.VALUE.DATE>
        Y.LD.INT = R.LD<LD.INTEREST.RATE>
        Y.LD.AMOUNT = R.LD<LD.AMOUNT>
        Y.LD.TERM = R.LD<LD.LOCAL.REF,POS5>
        CHANGE "M" TO "" IN Y.LD.TERM

        Y.COUNT = DCOUNT(R.LD.LOAIUD<LD.LOAIUD.T24.LOAN.CODE>,@VM)
        IF (Y.COUNT = 0) THEN
            Y.COUNT = 1
        END
        FOR I1 = 1 TO Y.COUNT
            Y.T24.CODE.UD = R.LD.LOAIUD<LD.LOAIUD.T24.LOAN.CODE,I1>
            Y.LSCS = R.LD.LOAIUD<LD.LOAIUD.LOCAL.REF,POS41>
            GOSUB GET.LSCS.PI
            Y.VAL.DATE = R.LD<LD.VALUE.DATE>
            IF (Y.LOAN.CODE EQ Y.T24.CODE.UD) OR (Y.T24.CODE.UD EQ "") THEN
                Y.FROM.TERM = R.LD.LOAIUD<LD.LOAIUD.FROM.TERM,I1>
                CHANGE "M" TO "" IN Y.FROM.TERM
                Y.TO.TERM = R.LD.LOAIUD<LD.LOAIUD.TO.TERM,I1>
                CHANGE "M" TO "" IN Y.TO.TERM
                Y.FROM.AM = R.LD.LOAIUD<LD.LOAIUD.FROM.AMT,I1>
                Y.TO.AM = R.LD.LOAIUD<LD.LOAIUD.TO.AMT,I1>
                IF (Y.FROM.TERM EQ "" AND Y.TO.TERM EQ "") OR (Y.FROM.TERM NE "" AND Y.TO.TERM NE "" AND Y.LD.TERM GE Y.FROM.TERM AND Y.LD.TERM LE Y.TO.TERM) THEN
                    IF (Y.FROM.AM EQ "" AND Y.TO.AM EQ "") OR (Y.FROM.AM NE "" AND Y.TO.AM NE "" AND Y.LD.AMOUNT GE Y.FROM.AM AND Y.LD.AMOUNT LE Y.TO.AM) THEN
                        YCHECK.UD = 0
                        Y.COUNT1 = DCOUNT(R.LD.LOAIUD<LD.LOAIUD.TERM,I1>,@SM)

                        FOR I2 = 1 TO Y.COUNT1
                            Y.TERM.UD = R.LD.LOAIUD<LD.LOAIUD.TERM,I1,I2>
                            Y.INT.RATE.UD = R.LD.LOAIUD<LD.LOAIUD.BIEN.DO,I1,I2>
                            Y.LS.UD.CD = R.LD.LOAIUD<LD.LOAIUD.MIN.INTEREST,I1,I2>

                            IF (Y.INT.RATE.UD EQ "") AND (Y.LS.UD.CD NE "") THEN
                                Y.LS.SS = Y.LS.UD.CD - Y.PI.RATE
                            END

                            IF (Y.LS.UD.CD EQ "") AND (Y.INT.RATE.UD NE "") THEN
                                Y.LS.SS = Y.INT.RATE.UD
                            END

                            IF (Y.LS.UD.CD EQ "") AND (Y.INT.RATE.UD EQ "") THEN
                                Y.LS.SS = 0
                            END

                            IF (Y.LS.UD.CD NE "") AND (Y.INT.RATE.UD NE "") THEN
                                Y.LS.UD.CD = Y.LS.UD.CD - Y.PI.RATE
                                Y.LS.SS = MAXIMUM(Y.INT.RATE.UD:@AM:Y.LS.UD.CD)
                            END

                            Y.TERM.UD = Y.TERM.UD["M",1,1]:"M"

                            CALL CALENDAR.DAY(Y.VAL.DATE,"+",Y.TERM.UD)
                            Y.VAL.DATE1 = Y.TERM.UD

******************************************************************************************************
*-------------------------- OPEN "MIG.DIR" TO MIG.DIR THEN END
*-------------------------- YLOG<-1> = Y.LD.ID:"#":Y.T24.CODE.UD:"#":Y.VAL.DATE:"#":Y.TERM.UD:"#":COMI:"#":TODAY:"#":Y.LD.INT
*-------------------------- WRITE YLOG ON MIG.DIR,"YLOG" ON ERROR END
******************************************************************************************************

                            IF (TODAY GE Y.VAL.DATE1) THEN
                                Y.VAL.DATE = Y.VAL.DATE1
                                CONTINUE
                            END

                            IF (TODAY LT Y.VAL.DATE1) THEN
                                YCHECK.UD = 1
                                BREAK
                            END

                        NEXT I2
                        IF (YCHECK.UD EQ 1) THEN
                            BREAK
                        END
                    END
                END
            END
        NEXT I1

        IF (YCHECK.UD = 1) THEN
            IF (LEFT(Y.LD.NGAY.LS1,8) GT TODAY) AND (Y.LD.NGAY.LS1 NE "") THEN
                RETURN        ;* 25/09/2017
            END
            Y.LS = Y.LS.SS
            Y.LD.NGAY.LS = Y.COMI
            GOSUB UPD.LS.LD.BD
        END

        IF (YCHECK.UD EQ 0) THEN
            Y.NGAY.THAY.LS = R.LD<LD.LOCAL.REF,20>
            Y.CUOI.LSUD = R.LD<LD.LOCAL.REF,POS1>
            IF (Y.CUOI.LSUD NE "") THEN
                CALL CDT(84,Y.CUOI.LSUD,1)
            END
            IF (LEN(Y.CUOI.LSUD) GT 8) THEN
                RETURN
            END
            IF (Y.CUOI.LSUD NE "" AND Y.CUOI.LSUD GT TODAY AND Y.VAL.DATE1 NE TODAY) OR (Y.CUOI.LSUD EQ "" AND Y.LD.NGAY.LS1 GT TODAY AND Y.VAL.DATE1 NE TODAY) THEN
                GOSUB UPD.NGAY.LS.BD
                RETURN
            END
            IF Y.VAL.DATE1 EQ TODAY OR (Y.PHAN.QUYEN.LS EQ "DUOC PHAN QUYEN LAI SUAT" AND Y.CUOI.LSUD EQ TODAY) OR ((Y.PHAN.QUYEN.LS EQ "" OR Y.PHAN.QUYEN.LS EQ "THAM GIA CHUONG TRINH") AND Y.LD.NGAY.LS1 LE TODAY) OR (Y.PHAN.QUYEN.LS EQ "DUOC PHAN QUYEN LAI SUAT" AND Y.CUOI.LSUD LT TODAY AND Y.LD.NGAY.LS1 LE TODAY) THEN
*-------------- IF (TODAY GE LEFT(COMI,8)) AND (LEFT(Y.NGAY.THAY.LS,8) LE TODAY) AND (Y.CUOI.LSUD LT TODAY) THEN    ;* 25/09/2017
                IF (COUNT("26020.01;12020.01;26040.01;12040.01",Y.LIMIT.REF) GT 0) THEN
                    GOSUB GET.LS.TC
                    GOSUB GET.LSCS.PI
                    Y.LS = Y.LS1
*------------------ BO SUNG -> DATE: 30/07/2017
                    IF (Y.LD.BIENDO.KYKE NE "") THEN
                        Y.LS = MAXIMUM(Y.LS1:@AM:Y.LD.BIENDO.KYKE)
                        Y.UPD = 2
                    END
                    Y.LD.NGAY.LS = Y.COMI
                    GOSUB UPD.LS.LD.BD
                END ELSE
                    IF (COUNT("26010.01;12010.01;26030.01;12030.01",Y.LIMIT.REF) GT 0) THEN
                        GOSUB GET.LS.TC
                        GOSUB GET.LSCS.PI
                        Y.LS = Y.LS2
*---------------------- BO SUNG -> DATE: 30/07/2017
                        IF (Y.LD.BIENDO.KYKE NE "") THEN
                            Y.LS = MAXIMUM(Y.LS2:@AM:Y.LD.BIENDO.KYKE)
                            Y.UPD = 2
                        END
                        Y.LD.NGAY.LS = Y.COMI
                        GOSUB UPD.LS.LD.BD
                    END
                END
            END
        END
    END

    IF (Y.UPD.CHECK EQ 1) THEN
        Y.LIST.LD1<-1> = Y.CO.CODE:"#":Y.LD.ID
    END
    IF (Y.UPD.CHECK EQ 0) THEN
        Y.LIST.LD2<-1> = Y.CO.CODE:"#":Y.LD.ID
    END

    RETURN

*-----------------------------------------------------------------------------
LD.SEND.MAIL:
*------------
    IF (Y.LIST.LD1 NE "") OR (Y.LIST.LD2 NE "") THEN
        Y.LIST.LD1 = SORT(Y.LIST.LD1)
        Y.LIST.LD2 = SORT(Y.LIST.LD2)
        Y.LD.SEL<-1> = Y.LIST.LD1
        Y.LD.SEL<-1> = Y.LIST.LD2
        Y.LD.SEL = SORT(Y.LD.SEL)

        OPEN "MIG.DIR/AC.LD.UPD.LS/DANHSACH" TO Y.PATH.DS THEN END
        READ R.MAIL FROM Y.PATH.DS,"KHCN.MAIL.TXT" THEN
            Y.DCOUNT.MAIL = DCOUNT(R.MAIL,@FM)
            FOR I = 1 TO Y.DCOUNT.MAIL
                Y.COMPANY = R.MAIL<I>["#",2,1]
                Y.MAIL = R.MAIL<I>["#",1,1]
                CHANGE " " TO "" IN Y.MAIL

*-------------- BEGIN: TRUNGNV - DATE: 09/11/2017
*-------------- LOC BO KY TU DAC BIET O CUOI CHUOI
                IF (SEQ(RIGHT(Y.MAIL,1)) LT 48) OR (SEQ(RIGHT(Y.MAIL,1)) GT 122) THEN
                    Y.MAIL = LEFT(Y.MAIL,LEN(Y.MAIL)-1)
                END
*-------------- END: TRUNGNV - DATE: 09/11/2017

                Y.LD.SEND1 = ""
                Y.LD.SEND2 = ""
                FINDSTR Y.COMPANY IN Y.LIST.LD1 SETTING POS1 THEN
                    Y.DCOUNT.LD1 = DCOUNT(Y.LIST.LD1,@FM)
                    FOR J = POS1 TO Y.DCOUNT.LD1
                        Y.LD.CO.CODE = Y.LIST.LD1<J>["#",1,1]
                        IF (Y.LD.CO.CODE NE Y.COMPANY) THEN
                            BREAK
                        END
                        Y.LD.SEND1 := Y.LIST.LD1<J>["#",2,1]:","
                    NEXT J
                END
                FINDSTR Y.COMPANY IN Y.LIST.LD2 SETTING POS1 THEN
                    Y.DCOUNT.LD2 = DCOUNT(Y.LIST.LD2,@FM)
                    FOR J = POS1 TO Y.DCOUNT.LD2
                        Y.LD.CO.CODE = Y.LIST.LD2<J>["#",1,1]
                        IF (Y.LD.CO.CODE NE Y.COMPANY) THEN
                            BREAK
                        END
                        Y.LD.SEND2 := Y.LIST.LD2<J>["#",2,1]:","
                    NEXT J
                END
                IF (Y.LD.SEND1 NE "") OR (Y.LD.SEND2 NE "") THEN
                    GOSUB SEND.MAIL     ;* GUI MAIL
                END
            NEXT I
        END
    END

    RETURN

*-----------------------------------------------------------------------------
SEND.MAIL:
*---------
    SUBJECT.MAIL = "Cap nhat lai suat tu dong"
    MESSAGE.MAIL = "Kinh gui anh/chi thong tin ve cac khoan vay da duoc cap nhat lai suat tu dong cua ngay lien ke hom truoc va cac khoan vay den han dieu chinh lai suat trong ngay hom nay nhu sau:     ":CHARX(13)
    MESSAGE.MAIL := "Ngay ":RIGHT(TODAY,2):"/":TODAY[5,2]:"/":LEFT(TODAY,4):" nhu sau:          ":CHARX(13)
*-- IF (Y.LD.SEND1 NE "") THEN
    MESSAGE.MAIL := "1. Danh sach cac khoan vay duoc cap nhat lai suat tu dong:  ":CHARX(13)
    MESSAGE.MAIL := Y.LD.SEND1:"          ":CHARX(13)
    MESSAGE.MAIL := "De nghi don vi kiem tra lai muc lai suat moi va thong bao den khach hang theo quy dinh hien hanh cua ABBANK          ":CHARX(13)
*-- END
*-- IF (Y.LD.SEND2 NE "") THEN
    MESSAGE.MAIL := "2. Danh sach cac khoan vay den han dieu chinh lai suat ":CHARX(13)
    MESSAGE.MAIL := Y.LD.SEND2:"          ":CHARX(13)
    MESSAGE.MAIL := "Don vi trao doi voi khach hang ve muc lai suat ap dung cho phu hop.    ":CHARX(13)
*-- END
    MESSAGE.MAIL := "          ":CHARX(13)
    MESSAGE.MAIL := "Best regards,"
    TO.MAIL = Y.MAIL

    GOSUB WRITE.MAIL.SEND

    T.RESULT = ABB.SEND.MAIL(TO.MAIL,SUBJECT.MAIL,MESSAGE.MAIL)
    CRT T.RESULT

    RETURN

*-----------------------------------------------------------------------------
WRITE.MAIL.SEND:
*---------------
    OPENSEQ Y.PATH,"LD.KHCN.MAIL.SENT.LIST.":Y.DATE:".TXT" TO FILE.POINTER ELSE
    END
    WRITESEQ TO.MAIL:"#":SUBJECT.MAIL:"#":MESSAGE.MAIL APPEND TO FILE.POINTER ELSE
        CRT "LOI KHONG GHI DUOC FILE !"
    END
    CLOSESEQ FILE.POINTER

    RETURN

*-----------------------------------------------------------------------------
GET.LS.TC:
*---------
    Y.LS1 = ""
    Y.LS2 = ""
    SEL1 = "SELECT ":FN.LD.INT.AUTO:" WITH T24.LOAN.CODE EQ '":Y.LOAN.CODE:"'"
    CALL EB.READLIST(SEL1,LIST.LD.INT,"","","")
    CALL F.READ(FN.LD.INT.AUTO,LIST.LD.INT<1>,R.LD.INT,FV.LD.INT.AUTO,"")
    IF R.LD.INT THEN
*------ Y.LS1 = R.LD.INT<ABB.LD.INT.AUTO.LOCAL.REF,POS21>   ;* LS TIN CHAP KY DC
*------ Y.LS2 = R.LD.INT<ABB.LD.INT.AUTO.LOCAL.REF,POS22>   ;* LS THE CHAP KY DC
        Y.LS1 = R.LD.INT<ABB.LD.INT.AUTO.LOCAL.REF,POS23>   ;* LS TIN CHAP KY DC
        Y.LS2 = R.LD.INT<ABB.LD.INT.AUTO.LOCAL.REF,POS24>   ;* LS THE CHAP KY DC
        Y.LSCS = R.LD.INT<ABB.LD.INT.AUTO.LOCAL.REF,POS25>  ;* LS CO SO
    END

    RETURN

*-----------------------------------------------------------------------------
UPD.LS.LD.BD:
*------------
*-- UPDATE LAI SUAT CHO LD
    BEGIN CASE
    CASE Y.UPD = 1
        IF (Y.LS NE "") AND (Y.LS NE 0) THEN
* S-T2420230802
            IF Y.LD.INT NE (Y.LS + Y.PI.RATE) THEN
				IF Y.LD.INT LT (Y.LS + Y.PI.RATE) THEN
					Y.UPD.CHECK = 1
					Y.LS.BD = Y.LS + Y.PI.RATE
					OFS.MSG = Y.VERSION:"/I/PROCESS,AUTOINT1.1/ABC$12345/":Y.CO.CODE:",":Y.LD.ID:","
					OFS.MSG := "NEW.INT.RATE::=":Y.LS.BD:",":"INT.RATE.V.DATE::=":TODAY

*-------------- UPDATE NGAY THAY LS KE TIEP THAY CHO GOSUB UPD.NGAY.LS.BD
					IF (LEN(Y.LD.NGAY.LS1) EQ 8) AND (Y.LD.NGAY.LS1 LE TODAY) THEN
						OFS.MSG := ",LOCAL.REF:20=":Y.LD.NGAY.LS
					END
					Y.OFS.LOG<-1> = OFS.MSG
				END ELSE
					Y.CHECK.T24.LC.EXIST = COUNT(Y.T24.LC.LIST.TO.SKIP, Y.LOAN.CODE)
					IF Y.CHECK.T24.LC.EXIST EQ 0 THEN
						Y.UPD.CHECK = 1
						Y.LS.BD = Y.LS + Y.PI.RATE
						OFS.MSG = Y.VERSION:"/I/PROCESS,AUTOINT1.1/ABC$12345/":Y.CO.CODE:",":Y.LD.ID:","
						OFS.MSG := "NEW.INT.RATE::=":Y.LS.BD:",":"INT.RATE.V.DATE::=":TODAY

*-------------- UPDATE NGAY THAY LS KE TIEP THAY CHO GOSUB UPD.NGAY.LS.BD
						IF (LEN(Y.LD.NGAY.LS1) EQ 8) AND (Y.LD.NGAY.LS1 LE TODAY) THEN
							OFS.MSG := ",LOCAL.REF:20=":Y.LD.NGAY.LS
						END
						Y.OFS.LOG<-1> = OFS.MSG
					END
				END
* E-T2420230802
            END ELSE
                GOSUB UPD.NGAY.LS.BD
            END
        END
    CASE Y.UPD = 2
        IF (Y.LS NE "") AND (Y.LS NE 0) THEN
            IF Y.LD.INT NE (Y.LS + Y.PI.RATE) THEN
                Y.UPD.CHECK = 1
                Y.LS.BD = Y.LS + Y.PI.RATE
                OFS.MSG = Y.VERSION:"/I/PROCESS,AUTOINT1.1/ABC$12345/":Y.CO.CODE:",":Y.LD.ID:","
                OFS.MSG := "NEW.INT.RATE::=":Y.LS.BD:",":"INT.RATE.V.DATE::=":TODAY

*-------------- UPDATE NGAY THAY LS KE TIEP THAY CHO GOSUB UPD.NGAY.LS.BD
                IF (LEN(Y.LD.NGAY.LS1) EQ 8) AND (Y.LD.NGAY.LS1 LE TODAY) THEN
                    OFS.MSG := ",LOCAL.REF:20=":Y.LD.NGAY.LS
                END
                Y.OFS.LOG<-1> = OFS.MSG
            END ELSE
                GOSUB UPD.NGAY.LS.BD
            END
        END
    END CASE

    RETURN

*-----------------------------------------------------------------------------
UPD.NGAY.LS.BD:
*--------------
*-- UPDATE NGAY THAY LAI SUAT KE TIEP BIEN DO
    IF (LEN(Y.LD.NGAY.LS1) GT 8) THEN
        RETURN
    END
    IF (Y.LD.NGAY.LS1 GT TODAY) THEN
        RETURN
    END
*-- HIEUDT1 (HIEU BOY) -> DATE: 24/02/2022 -> DESC: CAP NHAP DIEU KIEN CHO KHCN
    IF (UPCASE(Y.FLOATING.INT) NE "YES") THEN
        RETURN
    END
    Y.UPD.CHECK = 0
    OFS.MSG = Y.VERSION:"/I/PROCESS,AUTOINT1.1/ABC$12345/":Y.CO.CODE:",":Y.LD.ID:","
    OFS.MSG:= "LOCAL.REF:20=":Y.LD.NGAY.LS

    Y.OFS.LOG<-1> = OFS.MSG

    RETURN

*-----------------------------------------------------------------------------
GET.REF:
*-------
    LREF.APP = "LD.LOANS.AND.DEPOSITS":FM:"ABB.LD.INT.AUTO":FM:"AZ.ACCOUNT":FM:"ABB.LD.LOAI.UD.KHCN"
*S-T2420221130
    LREF.FLD = "NGAY.CUOI.LSUD":VM:"T24.LOAN.CODE":VM:"PHAN.QUYEN.LS":VM:"COLLATERAL.ID":VM:"TERM":VM:"LOAI.UU.DAI":VM:"BIEN.DO.KY.KE":VM:"ADDITIONAL.INFO":FM:"LS.TIN.CH.KY.DC":VM:"LS.THE.CH.KY.DC":VM:"LS.TIC.KY.DC.BD":VM:"LS.TC.KY.DC.BD":VM:"LAI.SUAT.CO.SO":FM:"TERM":FM:"LAI.SUAT.CO.SO":FM:"LAI.SUAT.CO.SO"
*E-T2420221130
    CALL MULTI.GET.LOC.REF(LREF.APP,LREF.FLD,L.POS)
    POS1 = L.POS<1,1>
    POS2 = L.POS<1,2>
    POS3 = L.POS<1,3>
    POS4 = L.POS<1,4>
    POS5 = L.POS<1,5>
    POS6 = L.POS<1,6>
    POS7 = L.POS<1,7>
*S-T2420221130
    POS8 = L.POS<1,8>
*E-T2420221130
    POS21 = L.POS<2,1>
    POS22 = L.POS<2,2>
    POS23 = L.POS<2,3>
    POS24 = L.POS<2,4>
    POS25 = L.POS<2,5>
    POS31 = L.POS<3,1>
    POS41 = L.POS<4,1>

    CALL EB.GET.LOC.POS(INT.POS,"LD.LOANS.AND.DEPOSITS","Floating Int")

    Y.VERSION = "LD.LOANS.AND.DEPOSITS,OFS"
    OFS.SOURCE = "OFS"

    RETURN

*-----------------------------------------------------------------------------
GET.LSCS.PI:
*-----------
*-- LAY LAI SUAT CO SO
*S-T2420221005
    Y.IS.HANDLER = Y.LSCS[1,6]
    IF Y.IS.HANDLER NE "LSTKCK" THEN
        CHANGE ',' TO '.' IN Y.LSCS
        Y.LSCS.BAK = Y.LSCS
        CHANGE '.' TO '' IN Y.LSCS

        IF ISDIGIT(Y.LSCS) EQ 1 THEN
            Y.PI.RATE = Y.LSCS.BAK
        END
        ELSE
            Y.PI.RATE = 0
        END
        RETURN
    END
*E-T2420221005
    Y.PI.ID = "01VND":TODAY
    CALL F.READ(FN.PI,Y.PI.ID,R.LS,FV.PI,"")
    Y.RES.PERIOD = R.LS<PI.REST.PERIOD>
    CHANGE @VM TO @FM IN Y.RES.PERIOD
    Y.RES.PERIOD = SPLICE("","~",Y.RES.PERIOD)
    Y.PI.RATE = ""
    FINDSTR "~":Y.LSCS["LSTKCK",2,1] IN Y.RES.PERIOD SETTING POS112 THEN
        Y.PI.RATE = R.LS<PI.BID.RATE,POS112,1>
    END
    IF (Y.LSCS = "") THEN
        Y.PI.RATE = 0
    END

    RETURN

*-----------------------------------------------------------------------------
RUN.OFS:
*-------
    Y.CO.CODE = OFS.MSG["/",5,1]
    Y.CO.CODE = Y.CO.CODE[",",1,1]

    OFS.MSG1 = I:"/":YC:"_":OFS.MSG
*-- OFS.MSG1 := "#":Y.LOAN.CODE

    OPENSEQ Y.PATH,"LD.UPD.LS.KHCN.":TODAY:".TXT" TO FILE.POINTER ELSE
    END
    WRITESEQ OFS.MSG1 APPEND TO FILE.POINTER ELSE
        CRT "LOI KHONG GHI DUOC FILE !"
    END
    CALL OFS.GLOBUS.MANAGER(OFS.SOURCE,OFS.MSG)
    WRITESEQ "KET QUA NHAP: ":OFS.MSG APPEND TO FILE.POINTER ELSE
        CRT "LOI KHONG GHI DUOC FILE !"
    END

    IF (OFS.MSG["/",3,1] GT 0) THEN
        OFS.MSG = Y.VERSION:"/A/PROCESS,AUTOINT2.1/ABC$12345/":Y.CO.CODE:",":OFS.MSG["//",1,1]
        WRITESEQ "AUTH: ":OFS.MSG APPEND TO FILE.POINTER ELSE
            CRT "LOI KHONG GHI DUOC FILE !"
        END
        CALL OFS.GLOBUS.MANAGER("OFS",OFS.MSG)
        WRITESEQ "KET QUA DUYET: ":OFS.MSG APPEND TO FILE.POINTER ELSE
            CRT "LOI KHONG GHI DUOC FILE !"
        END
    END

    CLOSESEQ FILE.POINTER

    RETURN
*-----------------------------------------------
*S-T2420221130
*-----------------------------------------------
*-   U P D A T E T 2 4 . L O A N . C 0 D E     -
*-----------------------------------------------
SUB.UPDATE.T24.LOAN.CODE:
    Y.ADDITIONAL.INFO.FLAG = 0
    Y.T24.CODE.FLAG = 0
    Y.T24.LC.LIST = "youSTAFF-TSBD":FM:"youSTAFF-ABB Care (TSBD)":FM:"CBNV-BDS.ND":FM:"CBNV-BDS.NDA":FM:"CBNV-BDS.XAYSUA":FM:"CBNV-OTO.TD.MOI":FM:"CBNV-OTO.TD.CU":FM:"CBNV-TSBD-TD.QTS TU HDLD":FM:"CBNV-TSBD-TD.TS KHAC":FM:"CBNV-KHONG TSBD-TD":FM:"youSTAFF-TSBD-NGUOI THAN":FM:"youSTAFF-TSBD-NGUOITHAN":FM:"youSTAFF-ABB Care (TSBD)-NGUOITHAN":FM:"CBNV-BDS.ND.NGUOI THAN":FM:"CBNV-BDS.NDA.NGUOI THAN":FM:"CBNV-BDS.XAYSUA.NGUOI THAN":FM:"CBNV-OTO.TD.MOI.NGUOI THAN":FM:"CBNV-OTO.TD.CU.NGUOI THAN":FM:"CBNV-TSBD-TD.NGUOI THAN":FM:"CBNV-KHONG TSBD-TD.NGUOI THAN":FM:"youSTAFF-KHONG TSBD":FM:"youSTAFF-ABB Care (Khong TSBD)":FM:"youSTAFF-KHONG TSBD-NGUOI THAN":FM:"youSTAFF-KHONG TSBD-NGUOITHAN":FM:"youSTAFF-ABBCare(Ko TSBD)-NGUOITHAN"
    Y.NO.OF.T24.LC = DCOUNT(Y.T24.LC.LIST, FM)
    FOR I.LD.T24.LC = 1 TO Y.NO.OF.T24.LC
        Y.T24.LC.VAL = Y.T24.LC.LIST<I.LD.T24.LC>
        IF Y.LOAN.CODE EQ Y.T24.LC.VAL THEN
            Y.T24.CODE.FLAG = 1
            BREAK
        END
    NEXT I.LD.T24.LC

    Y.NO.OF.ADD.INFO = DCOUNT(Y.ADDITIONAL.INFO, @SM)
    FOR I.LD.ADD.INFO = 1 TO Y.NO.OF.ADD.INFO
        Y.ADD.INFO.VAL = Y.ADDITIONAL.INFO<1,1,I.LD.ADD.INFO>
        IF Y.ADD.INFO.VAL EQ "VAY UU DAI CBNV ABBANK" THEN
            Y.ADDITIONAL.INFO.FLAG = 1
            BREAK
        END
    NEXT I.LD.ADD.INFO

    IF Y.T24.CODE.FLAG EQ 0 AND Y.ADDITIONAL.INFO.FLAG EQ 1 AND (Y.LIMIT.REF EQ "10101.01" OR Y.LIMIT.REF EQ "10108.01" OR Y.LIMIT.REF EQ "12010.01" OR Y.LIMIT.REF EQ "12030.01" OR Y.LIMIT.REF EQ "26010.01" OR Y.LIMIT.REF EQ "26030.01") THEN
        Y.LOAN.CODE = "youSTAFF-TSBD"
    END

    IF Y.T24.CODE.FLAG EQ 0 AND Y.ADDITIONAL.INFO.FLAG EQ 1 AND (Y.LIMIT.REF EQ "10102.01" OR Y.LIMIT.REF EQ "12020.01" OR Y.LIMIT.REF EQ "26020.01") THEN
        Y.LOAN.CODE = "youSTAFF-KHONG TSBD"
    END
* S-T2420230307
    Y.ADD.INFO.EXIST = COUNT(Y.ADDITIONAL.INFO, "CBNV GELEXIMCO NGHI VIEC")
    IF (Y.LOAN.CODE EQ "GELEXIMCO STAFF-KHONG TSBD" OR Y.LOAN.CODE EQ "GELEXIMCO STAFF-CTTV-KHONG TSBD" OR Y.LOAN.CODE EQ "GELEXIMCO STAFF") AND Y.ADD.INFO.EXIST GE 1 THEN
        Y.LOAN.CODE = "youMONEY"
    END
* S-T2420230307

    RETURN
*-----------------------------------------------
*E-T2420221130
*-----------------------------------------------

END
