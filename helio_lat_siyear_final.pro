pro helio_lat_siyear_final
;cd, 'F:\susanta_backup_Nov2014\winf32\2014\IPS\15-si-year'
cd,'/home/susanta/Desktop/JERRY-PAPER/analysis/IPS/23-si-year/'
;-
;- Make plot  Maunder minimum paper.
;- Update the results of grl paper using new data sets for the year 2010,2011, 2012, 2013.
;- Program is adopted from folder \IPS\7-si-year
;-------------------------------------------------------------------------------------
;- Progam to plot sc.index vs year for more than one source (multi-source).
;- First select all observations in the elongation range 15-55 deg.
;- Then normalize sc.index values for each source at each elongation with
;- those for 1148-001 to make them distance indepedent.
;-
;- Put a sigma cutoff to remove unnecessary high points in sc. index data.
;- Prior to that, normalize these sc.index data with the maximum value.
;- Subsequently, for each source we can divide the scindex data into set of
;- observations at high and low latitudes.
;-
;- For each such divided dataset of high and low latitudes,
;- 1. Compute the average of sc.index for each year.
;- 2. Compute the drop in sc.index values.
;-
;- Finally, plot for each source scindex observations with year for the full
;- dataset overlapped with only high latiude observations. Also overplot the
;- annual means of scindex values for the full data sets.
;-----------------------------------------------------------------------------------
;-
;- To run the program : .r helio_lat_siyear_final
;- To compile the program : helio_lat_siyear_final
;- Input file = 'vlist1983-2017-new-03.dat' ;- Updated data file upto 2016.
;- Output file = 'norm-scindex-'+ts+'-1.eps' ;- 27-source plot.
;- Output file ='annual-means-plot.eps'
;- Output file ='annual-means-plot-2.eps'
;- Output file ='annual-means-plot-3.eps'
;- Output file ='annual-means-plot-nature.eps'
;- Output file ='annual-means-plot-ApJL-bw.eps'
;===============INPUTS FROM TERMINAL=============================================================
;- Group observations into observations at low and high latitudes ( Give inputs for the cutoff of
;- latitude range).
;- Give inputs for rows and columns to produce multiplot.
;- Give inputs for number of sources to plot.


gamma1=45        ;gamma1=lower cutoff for latitude observations
gamma2=90        ;gamma2=higher cutoff for latitude observations
gamma3=0         ;gamma3=lowest cutoff for latitude observations
maxpts=100       ;points cutoff to plot
m=3              ;m=no. of rows
n=9              ;n=no. of columns
n_sources=27     ;n_sources=no. of sources

;----------------------------------LONG INTEGERS----------------------------------------------------
;To convert short default integers into long integers use

Compile_Opt DEFINT32

;======================= Selection and normalization of sc.index ===================================

;- Read scindex data file and select the observations in 15-55 eps range.
;- Once selected, then normalize scindex values at every eps with corresponding
;- values of scindex for 1148-001 (a nearly point source) to make distance independant.
;- Compute the fractional years.
;- Then read the source file.

;-------------------------------------Read data file-----------------------------------------------

;- No. of rows to read "vlist1983-2017-new-03.dat"               ;- Updated IPS data file

   infile='vlist1983-2017-new-03.dat'
   n_rows=file_lines(infile)

;- Defining variables to read data in 'vlist1983-2013-new-03.dat'

  date=""                                    ;DATE
  p1=""                                      ;souRCE
  p2=0.0                                     ;UT
  p3=0.0                                     ;DIST
  p4=0.0                                     ;LATH
  p5=0.0                                     ;LONGH
  p6=0.0                                     ;LATG
  p7=0.0                                     ;LONGG
  p8=0.0                                     ;CARR
  p9=0.0                                     ;V
  p10=0.0                                    ;ER
  p11=0.0                                    ;SC-INDEX

;- Defining new variables to store data.

             year=STRARR(n_rows)
             sou=STRARR(n_rows)
             UT=FLTARR(n_rows)
             DIS=FLTARR(n_rows)
             HLA=INTARR(n_rows)
             HLO=INTARR(n_rows)
             GLA=INTARR(n_rows)
             GLO=INTARR(n_rows)
             CARR=INTARR(n_rows)
             V=INTARR(n_rows)
             ERR=INTARR(n_rows)
             SCINDEX=DBLARR(n_rows)

;- Read data file.

             count=0

             OPENR, LUN, infile,/GET_LUN
                 WHILE (~ EOF(lun)) DO BEGIN
                   READF, lun,date,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11, $
                   format='(a8,2x,a8,2x,f5.2,2x,f4.2,2x,i3,2x,i4,2x,i3,2x,i4,2x,i4,2x,i4,2x,i4,2x,e12.3)'

                         year(count)=date
                         sou(count)=p1
                         UT(count)=p2
                         DIS(count)=p3
                         HLA(count)=p4
                         HLO(count)=p5
                         GLA(count)=p6
                         GLO(count)=p7
                         CARR(count)=p8
                         V(count)=p9
      			 ERR(count)=p10
      			 SCINDEX(count)=p11

                   count=count+1
                 ENDWHILE

     			year=year(0:count-1)
     			sou=strcompress(sou(0:count-1),/remove_all)             ;- To remove extra space in the string
     			UT=UT(0:count-1)
     			DIS=DIS(0:count-1)
     			HLA=HLA(0:count-1)
     			HLO=HLO(0:count-1)
     			GLA=GLA(0:count-1)
     			GLO=GLO(0:count-1)
     			CARR=CARR(0:count-1)
     			V=V(0:count-1)
     			ERR=ERR(0:count-1)
     			SCINDEX=SCINDEX(0:count-1)

	     FREE_LUN,LUN
;------------------------------------------constant---------------------------------------------------------------

        pi=4.0*atan(1.0)                                     ;- assigning pi value
        dtr=pi/180.0                                         ;- degree to radian
        rtd=180.0/pi                                         ;- radian to degree

;-----------------------------------observations in eps range 15-55 deg-------------------------------------------

;- Select sources in the elong range 15 to 55 deg
;-       this corresponds to Distance of 0.26 and 0.82 AU

        eps1=asin(0.26)*rtd     ; elong=15 deg              ;- comment while using inputs from terminal
;        eps2=asin(0.34)*rtd     ; elong=20 deg
;        eps1=asin(0.34)*rtd     ; elong=20 deg
;        eps2=asin(0.42)*rtd     ; elong=25 deg
;        eps1=asin(0.42)*rtd     ; elong=25 deg
;        eps2=asin(0.50)*rtd     ; elong=30 deg
;        eps1=asin(0.50)*rtd     ; elong=30 deg
;        eps2=asin(0.57)*rtd     ; elong=35 deg
;        eps1=asin(0.57)*rtd     ; elong=35 deg
;        eps2=asin(0.64)*rtd     ; elong=40 deg
;        eps1=asin(0.64)*rtd     ; elong=40 deg
;        eps2=asin(0.71)*rtd     ; elong=45 deg
;        eps1=asin(0.71)*rtd     ; elong=45 deg
;        eps2=asin(0.77)*rtd     ; elong=50 deg
;        eps1=asin(0.77)*rtd     ; elong=50 deg
        eps2=asin(0.82)*rtd     ;elong=55 deg               ;- comment while using inputs from terminal


Print,'Solar elongation range in deg=',eps1,' to ',eps2

        eps3=sin(eps1*dtr)
        eps4=sin(eps2*dtr)

Print,'Distance from sun in au=',eps3,' to ',eps4

;- New variables to accommodate the selected values.

     			nyear=strarr(n_rows)
     			nsou=strarr(n_rows)
     			nUT=fltarr(n_rows)
     			nDIS=fltarr(n_rows)
     			nHLA=intarr(n_rows)
     			nHLO=intarr(n_rows)
     			nGLA=intarr(n_rows)
     			nGLO=intarr(n_rows)
     			nCARR=intarr(n_rows)
     			nV=intarr(n_rows)
     			nERR=intarr(n_rows)
     			nSCINDEX=dblarr(n_rows)


        		ncnti=0
        			for p=0,count-1 do begin
              				if(scindex(p) ne 0.0 && dis(p) ge eps3 && dis(p) lt eps4) then begin
                				nyear(ncnti)=year(p)
               					nsou(ncnti)=sou(p)
                				nUT(ncnti)=UT(p)
                				nDIS(ncnti)=DIS(p)
                				nHLA(ncnti)=HLA(p)
                				nHLO(ncnti)=HLO(p)
                				nGLA(ncnti)=GLA(p)
                				nGLO(ncnti)=GLO(p)
                				nCARR(ncnti)=CARR(p)
                				nV(ncnti)=V(p)
                				nERR(ncnti)=ERR(p)
                				nSCINDEX(ncnti)=SCINDEX(p)
                				ncnti=ncnti+1
              				endif
        			endfor

              		nyear=nyear(0:ncnti-1)
              		nsou=strcompress(nsou(0:ncnti-1),/remove_all)           ;- To remove extra space in the string
              		nUT=nUT(0:ncnti-1)
              		nDIS=nDIS(0:ncnti-1)
              		nHLA=nHLA(0:ncnti-1)
              		nHLO=nHLO(0:ncnti-1)
              		nGLA=nGLA(0:ncnti-1)
              		nGLO=nGLO(0:ncnti-1)
              		nCARR=nCARR(0:ncnti-1)
              		nV=nV(0:ncnti-1)
              		nERR=nERR(0:ncnti-1)
              		nSCINDEX=nSCINDEX(0:ncnti-1)

;-------------------------------------------- Normalization and Computing Fractional year --------------------------------------
;-    Extract yr,mon and day from nyear
;-    Define variables before looping
;-    Fractional day conversion

       			yr=strarr(n_rows)
        		mon=strarr(n_rows)
         		day=strarr(n_rows)
           		fdd=fltarr(n_rows)

     			for i=0,ncnti-1 do begin
            				yr(i) =  ( StrMid( StrTrim(nyear(i), 2), 0, 2) )
              				mon(i) = ( StrMid( StrTrim(nyear(i), 2), 2, 2) )
                			day(i) =   ( StrMid( StrTrim(nyear(i), 2), 4, 2) )

                    			fdd(i)=float(day(i))+(nut(i)/24.0)

;-    Normalize scintillation index by that of 1148-001 at the same distance.
;-    Normalized facors are used from a file /ips/14-1148/avg1148-001.dat.
							   if ndis(i) eq 0.26 then nscindex(i)=nscindex(i)/6.008e+02
							   if ndis(i) eq 0.27 then nscindex(i)=nscindex(i)/6.070e+02
							   if ndis(i) eq 0.28 then nscindex(i)=nscindex(i)/5.121e+02
							   if ndis(i) eq 0.29 then nscindex(i)=nscindex(i)/5.564e+02
							   if ndis(i) eq 0.30 then nscindex(i)=nscindex(i)/7.032e+02
							   if ndis(i) eq 0.31 then nscindex(i)=nscindex(i)/5.465e+02
							   if ndis(i) eq 0.32 then nscindex(i)=nscindex(i)/4.143e+02
							   if ndis(i) eq 0.33 then nscindex(i)=nscindex(i)/4.071e+02
							   if ndis(i) eq 0.34 then nscindex(i)=nscindex(i)/4.212e+02
							   if ndis(i) eq 0.35 then nscindex(i)=nscindex(i)/4.435e+02
							   if ndis(i) eq 0.36 then nscindex(i)=nscindex(i)/3.266e+02
							   if ndis(i) eq 0.37 then nscindex(i)=nscindex(i)/3.572e+02
							   if ndis(i) eq 0.38 then nscindex(i)=nscindex(i)/3.531e+02
							   if ndis(i) eq 0.39 then nscindex(i)=nscindex(i)/2.544e+02
							   if ndis(i) eq 0.40 then nscindex(i)=nscindex(i)/2.900e+02
							   if ndis(i) eq 0.41 then nscindex(i)=nscindex(i)/3.223e+02
							   if ndis(i) eq 0.42 then nscindex(i)=nscindex(i)/2.624e+02
							   if ndis(i) eq 0.43 then nscindex(i)=nscindex(i)/2.816e+02
							   if ndis(i) eq 0.44 then nscindex(i)=nscindex(i)/2.866e+02
							   if ndis(i) eq 0.45 then nscindex(i)=nscindex(i)/2.572e+02
							   if ndis(i) eq 0.46 then nscindex(i)=nscindex(i)/2.633e+02
							   if ndis(i) eq 0.47 then nscindex(i)=nscindex(i)/2.059e+02
							   if ndis(i) eq 0.48 then nscindex(i)=nscindex(i)/1.523e+02
							   if ndis(i) eq 0.49 then nscindex(i)=nscindex(i)/2.785e+02
							   if ndis(i) eq 0.50 then nscindex(i)=nscindex(i)/2.276e+02
							   if ndis(i) eq 0.51 then nscindex(i)=nscindex(i)/1.337e+02
							   if ndis(i) eq 0.53 then nscindex(i)=nscindex(i)/2.028e+02
							   if ndis(i) eq 0.54 then nscindex(i)=nscindex(i)/2.024e+02
							   if ndis(i) eq 0.55 then nscindex(i)=nscindex(i)/1.212e+02
							   if ndis(i) eq 0.56 then nscindex(i)=nscindex(i)/1.310e+02
							   if ndis(i) eq 0.57 then nscindex(i)=nscindex(i)/1.010e+02
							   if ndis(i) eq 0.58 then nscindex(i)=nscindex(i)/2.053e+02
							   if ndis(i) eq 0.60 then nscindex(i)=nscindex(i)/1.421e+02
							   if ndis(i) eq 0.61 then nscindex(i)=nscindex(i)/2.077e+02
							   if ndis(i) eq 0.62 then nscindex(i)=nscindex(i)/7.933e+01
							   if ndis(i) eq 0.63 then nscindex(i)=nscindex(i)/9.243e+01
							   if ndis(i) eq 0.64 then nscindex(i)=nscindex(i)/7.303e+01
							   if ndis(i) eq 0.65 then nscindex(i)=nscindex(i)/7.831e+01
							   if ndis(i) eq 0.66 then nscindex(i)=nscindex(i)/8.421e+01
							   if ndis(i) eq 0.67 then nscindex(i)=nscindex(i)/8.523e+01
							   if ndis(i) eq 0.68 then nscindex(i)=nscindex(i)/1.148e+02
							   if ndis(i) eq 0.69 then nscindex(i)=nscindex(i)/4.120e+01
							   if ndis(i) eq 0.70 then nscindex(i)=nscindex(i)/1.263e+02
							   if ndis(i) eq 0.71 then nscindex(i)=nscindex(i)/8.877e+01
							   if ndis(i) eq 0.72 then nscindex(i)=nscindex(i)/1.188e+02
							   if ndis(i) eq 0.73 then nscindex(i)=nscindex(i)/7.520e+01
							   if ndis(i) eq 0.74 then nscindex(i)=nscindex(i)/9.348e+01
							   if ndis(i) eq 0.75 then nscindex(i)=nscindex(i)/4.318e+01
							   if ndis(i) eq 0.76 then nscindex(i)=nscindex(i)/4.215e+01
							   if ndis(i) eq 0.77 then nscindex(i)=nscindex(i)/6.665e+01
							   if ndis(i) eq 0.78 then nscindex(i)=nscindex(i)/3.910e+01
							   if ndis(i) eq 0.79 then nscindex(i)=nscindex(i)/9.810e+01
							   if ndis(i) eq 0.80 then nscindex(i)=nscindex(i)/5.207e+01
							   if ndis(i) eq 0.81 then nscindex(i)=nscindex(i)/5.205e+01
							   if ndis(i) eq 0.82 then nscindex(i)=nscindex(i)/4.300e+01
     			endfor

;----------------------------------- Converting 83 to 1983 and 01 to 2001------------------------------------------------

					       for j=0,ncnti-1 do begin
							    if(yr(j) gt 80)then begin
							      		yr(j)=yr(j)+1900
							    	endif else begin
							     		yr(j)=yr(j)+2000
							    endelse
					       endfor

;-----------------------------------Compute fractional year 'fry'- eg july 1 1983 will be 1983.498-----------------------
			   fry=fltarr(n_rows)
			      for k=0,ncnti-1 do begin
				 if ( (float(yr(k)) mod 400. eq 0) || (float(yr(k)) mod 100. ne 0) && (float(yr(k)) mod 4. eq 0)) then begin
					  if  mon(k) eq  01 then fry(k)=float(yr(k))+(fdd(k)/366.0)
					  if  mon(k) eq  02 then fry(k)= float(yr(k))+((fdd(k)+31)/366.0)
					  if  mon(k) eq  03 then fry(k)=float(yr(k))+((fdd(k)+60)/366.0)
					  if  mon(k) eq  04 then fry(k)=float(yr(k))+((fdd(k)+91)/366.0)
					  if  mon(k) eq  05 then fry(k)=float(yr(k))+((fdd(k)+121)/366.0)
					  if  mon(k) eq  06 then fry(k)=float(yr(k))+((fdd(k)+152)/366.0)
					  if  mon(k) eq  07 then fry(k)=float(yr(k))+((fdd(k)+182)/366.0)
					  if  mon(k) eq  08 then fry(k)=float(yr(k))+((fdd(k)+213)/366.0)
					  if  mon(k) eq  09 then fry(k)=float(yr(k))+((fdd(k)+244)/366.0)
					  if  mon(k) eq  10 then fry(k)=float(yr(k))+((fdd(k)+274)/366.0)
					  if  mon(k) eq  11 then fry(k)=float(yr(k))+((fdd(k)+305)/366.0)
					  if  mon(k) eq  12 then fry(k)=float(yr(k))+((fdd(k)+335)/366.0)
			       endif else begin
					  if  mon(k) eq  01 then fry(k)=float(yr(k))+(fdd(k)/365.0)
					  if  mon(k) eq  02 then fry(k)= float(yr(k))+((fdd(k)+31)/365.0)
					  if  mon(k) eq  03 then fry(k)=float(yr(k))+((fdd(k)+59)/365.0)
					  if  mon(k) eq  04 then fry(k)=float(yr(k))+((fdd(k)+90)/365.0)
					  if  mon(k) eq  05 then fry(k)=float(yr(k))+((fdd(k)+120)/365.0)
					  if  mon(k) eq  06 then fry(k)=float(yr(k))+((fdd(k)+151)/365.0)
					  if  mon(k) eq  07 then fry(k)=float(yr(k))+((fdd(k)+181)/365.0)
					  if  mon(k) eq  08 then fry(k)=float(yr(k))+((fdd(k)+212)/365.0)
					  if  mon(k) eq  09 then fry(k)=float(yr(k))+((fdd(k)+243)/365.0)
					  if  mon(k) eq  10 then fry(k)=float(yr(k))+((fdd(k)+273)/365.0)
					  if  mon(k) eq  11 then fry(k)=float(yr(k))+((fdd(k)+304)/365.0)
					  if  mon(k) eq  12 then fry(k)=float(yr(k))+((fdd(k)+334)/365.0)
			     endelse
			   endfor
;------------------------------------------- READ SOURCE FILE----------------------------------------------------------------
;- READING SOURCE list

     n_lines=file_lines('source-27.lis')

     sour=""
     source=strarr(n_lines)
     cnt=0
     OPENR, lun,'source-27.lis',/GET_LUN
        WHILE (~ EOF(lun)) DO BEGIN
          READF, lun,sour,format='(a12)'
           source(cnt)=sour
            cnt=cnt+1
        ENDWHILE
          source=STRCOMPRESS(source(0:cnt-1),/REMOVE_ALL)
    FREE_LUN,LUN

;==================================PLOT SC. INDXE VS YEAR===============================================================

;- Idea is to create two dimensional array saving observed values for each variables corresponding to every source.
;- Define the plotting enviornment.
;- Compute new scindex values and frac. year for each source by discarding those above 2 sigma after normalizing with
;- the maximum scindex for each source.
;- Group scindex values into heliolatitude ranges.
;- Compute the annual means in each heliolatitude ranges.
;- Finally plot low and high heliolatitude observations variation with time
;- Overplot the yearly means for low heliolatitude observations.

;--------------------------------------Assigning observations for each source into an array------------------------------------------------

        		    n_source = cnt                 	  ;- no. of sources

					sindx=dblarr(100000,n_source)
					fry2=dblarr(100000,n_source)
					DIS2=fltarr(100000,n_source)
					HLA2=intarr(100000,n_source)
					HLO2=intarr(100000,n_source)
					GLA2=intarr(100000,n_source)
					GLO2=intarr(100000,n_source)
					CARR2=intarr(100000,n_source)
					V2=intarr(10000,n_source)
					ERR2=intarr(10000,n_source)
        				ncnt_arr = lonarr(n_source)

					     for i_source=0,n_source-1 do begin                                  ;- loop over each source
						 ncnt=0                                                              ;- initialise no. of points per source
						 for t=0,ncnti-1 do begin
						       if((nsou(t)) eq (source(i_source)) && (nscindex(t)) ne 0.0 ) then begin
						       sindx(ncnt,i_source)=(alog10(10e+4*nscindex(t)))              ;- Save values to new array.
						       fry2(ncnt,i_source)=fry(t)
						       DIS2(ncnt,i_source)=nDIS(t)
						       HLA2(ncnt,i_source)=nHLA(t)
						       HLO2(ncnt,i_source)=nHLO(t)
						       GLA2(ncnt,i_source)=nGLA(t)
						       GLO2(ncnt,i_source)=nGLO(t)
						       CARR2(ncnt,i_source)=nCARR(t)
						       V2(ncnt,i_source)=nV(t)
						       ERR2(ncnt,i_source)=nERR(t)
							ncnt=ncnt+1
						      endif
						 endfor
						      ncnt_arr[i_source] = ncnt
					     endfor
						       sindx=sindx[0:max(ncnt_arr)-1,*]
						       fry2=fry2[0:max(ncnt_arr)-1,*]
						       DIS2=DIS2[0:max(ncnt_arr)-1,*]
						       HLA2=HLA2[0:max(ncnt_arr)-1,*]
						       HLO2=HLO2[0:max(ncnt_arr)-1,*]
						       GLA2=GLA2[0:max(ncnt_arr)-1,*]
						       GLO2=GLO2[0:max(ncnt_arr)-1,*]
						       CARR2=CARR2[0:max(ncnt_arr)-1,*]
						       V2=V2[0:max(ncnt_arr)-1,*]
						       ERR2=ERR2[0:max(ncnt_arr)-1,*]

;----------------------------------------- Define arrays---------------------------------------------------------------------

;- Arrays for maximum, mean and rms of scindex computation.

              	simax=dblarr(n_source)
              	simean=dblarr(n_source)
              	si_sigma=dblarr(n_source)

;- Arrays for sigma cutoff.

              	y1=dblarr(n_source)
              	y2=dblarr(n_source)
		y3=dblarr(n_source)
              	y4=dblarr(n_source)
;- Arrays for plotting annual means

                data1=fltarr(33,n_sources)                                         ;- array for annual years for each source
		data2=fltarr(33,n_sources)                                         ;- array for annual means for each source
		ndata_arr=lonarr(n_sources)                                        ;- no. of obs. for each source

;---------------------------------------- BEGIN MULTIPLOT ENVIORNMENT----------------------------------------------------------------

      		SET_PLOT,'PS'
			nplot=0                                                      ;- Initialise plot number.
                        npanel=0                                                     ;- Intitialise Panel number.
			yval=0                                                       ;- Initialise value for labeling y-axis.
			nplt=fix(m*n)                                                ;- Define no. of plots per page.

		    t=1                                                              ;- Initialise plot-name
			ts=strcompress(t,/remove_all)
			;outfile='norm-scindex-'+ts+'.eps'                           ;- without naming x-axis labels.
			outfile='norm-scindex-'+ts+'-lablled.eps'                          ;- with naming x-axis labels.

				device,filename=outfile,xsize=35,ysize=70,bits=64, $
					   scale_factor=1.0,/encapsulated,/color,yoffset=0.1,xoffset=0.1

				!p.multi=[0,m,n]                                         ;- Set no. of plots per page.
				!p.charthick=10                                          ;- Set global char. thickness.

				multiplot,xgap=0.005,ygap=0.0025, $                      ;- Call multiplot routine.
					     mytitle='Normalized Scintillation Index (m)', myTitSize=2.6, myTitOffset=1.2, $
					     mxtitle='Year', mxTitSize=2.1, mxTitOffset=1.2



;-----------------------------------------------PLOT CONDITIONS----------------------------------------------------------------------

			for i_source=0,n_sources-1 do begin                               ;- Loop for each source

   			   if (ncnt_arr[i_source] gt maxpts) then begin  		          ;- Set condition for minimum no. of observations.

;----------------------------------------------SIGMA CUTOFF --------------------------------------------------------------------------
         	    simax[i_source]=max(sindx[0:ncnt_arr[i_source]-1,i_source])                ;- Computes maxima of a given array

                            npts=ncnt_arr[i_source]

               			    for i=0,npts-1 do begin
                      			sindx(i,i_source)=sindx(i,i_source)/float(simax(i_source)) ;- Normalise with the maximum value
            			    endfor

				sindx2=dblarr(npts,n_sources)
				ffry=dblarr(npts,n_sources)
				ngla2=dblarr(npts,n_sources)
				ndis2=dblarr(npts,n_sources)

				sindx2(*,i_source)=sindx(0:npts-1,i_source)
				ffry(*,i_source)=fry2(0:npts-1,i_source)
				ngla2(*,i_source)=gla2(0:npts-1,i_source)
				ndis2(*,i_source)=dis2(0:npts-1,i_source)

				simean[i_source]=mean(sindx2[*,i_source])                        ;- Computes the mean value of a given array

				si_sigma[i_source]=(sqrt(variance(sindx2[*,i_source])))          ;- Find RMS

;- Create new arrays for fry and sindx by dropping all points above 2 sigma

					
					  y1[i_source]=(simean[i_source]+((si_sigma[i_source])*2.0))
					  y2[i_source]=(simean[i_source]-((si_sigma[i_source])*2.0))
					
					   y3[i_source]=0.65*(simean[i_source]+((si_sigma[i_source])*2.0))
					   y4[i_source]=(simean[i_source]-((si_sigma[i_source])*2.0))
					

					  newarr=dblarr(4,npts)
                                        
					   ss=0
					   for i=0,npts-1 do begin
                                                if (ffry(i,i_source) le 2008) then begin
						 if (sindx2(i,i_source) gt y2[i_source] && sindx2(i,i_source) le y1[i_source] ) then begin
						      newarr(0,ss)=ffry(i,i_source)               ;- fractional year
						      newarr(1,ss)=sindx2(i,i_source)             ;- normalised sc-index
						      newarr(2,ss)=ngla2(i,i_source)              ;- heliographic latitude
						      newarr(3,ss)=ndis2(i,i_source)              ;- distance from the sun
						    ss=ss+1
						 endif
                                                endif else begin
						 if (sindx2(i,i_source) gt y4[i_source] && sindx2(i,i_source) le y3[i_source] ) then begin
						      newarr(0,ss)=ffry(i,i_source)               ;- fractional year
						      newarr(1,ss)=sindx2(i,i_source)             ;- normalised sc-index
						      newarr(2,ss)=ngla2(i,i_source)              ;- heliographic latitude
						      newarr(3,ss)=ndis2(i,i_source)              ;- distance from the sun
						    ss=ss+1
						 endif
                                               endelse
					   endfor

Print,'no. of points to plot before applying rms cutoff:',npts
Print,'no. of points to plot after applying rms cutoff:',ss

;- Array for plotting

			      xx=fltarr(ss)
			      yy=fltarr(ss)
			      xxl=fltarr(ss)
			      xxd=fltarr(ss)

			      xx=newarr(0,0:ss-1)
			      yy=newarr(1,0:ss-1)
			      xxl=newarr(2,0:ss-1)
			      xxd=newarr(3,0:ss-1)

;- max,min of array for plotting
				    ymax=max(yy)
				    ymin=min(yy)

				    dy=(ymax-ymin)/10.0

;-------------------------------------------GROUPING OBS. FOR HIGH LATITUDE SOURCES -------------------------------------------------
;- If the source have both high and low heliolatitude observations, then group them into high and low helio-latitude.
;- Compute the annual means for these high and low latitude observations.

     			 if(max(abs(xxl)) gt 45) then begin       ;- condition for high helio-latitude selection

;---------------------------------------------HIGH HELIO-LATITUDE----------------------------------------------------------------------
				    xx2=fltarr(ss)
				    yy2=fltarr(ss)
				    xxl2=fltarr(ss)
				    xxd2=fltarr(ss)
				    ss2=0

				   for i=0,ss-1 do begin
					  if (abs(xxl(i)) ge gamma1 && abs(xxl(i)) lt gamma2) then begin
					    xx2(ss2)=xx(i)                     ;- fractional year
					    yy2(ss2)=yy(i)
					    xxl2(ss2)=xxl(i)                   ;- heliographic latitude
					    xxd2(ss2)=xxd(i)                   ;- distance
					    ss2=ss2+1
					 endif
				   endfor

				   xx2=xx2(0:ss2-1)
				   yy2=yy2(0:ss2-1)
				   xxl2=xxl2(0:ss2-1)
				   xxd2=xxd2(0:ss2-1)

;- Computing avgyear

				     xxn2=fltarr(10000)
				     yyn2=fltarr(10000)

				jj2=0

				   for j2=1983,2017,1 do begin                                             ;- loop over all years

					 ny=j2

					   sumx2=0.0
					   sumy2=0.0
					 ncount2=0                                                         ;- initilize no of pts per year

				       for i=0,ss2-1 do begin
					  if(fix(xx2(i)) eq ny)then begin
						sumx2=sumx2+xx2(i)
					       sumy2=sumy2+yy2(i)
					     ncount2=ncount2+1
					  endif
				       endfor
					    if(ncount2 ne 0)then begin

					       xxn2(jj2)=sumx2/float(ncount2)
						yyn2(jj2)=sumy2/float(ncount2)
					      jj2=jj2+1
					    endif
				   endfor


				   xxn2=xxn2(0:jj2-1)
				   yyn2=yyn2(0:jj2-1)

				  highl=(ss2*100/float(ss))                           ;- Relative no. of high latitude observations.
				  highl=fix(highl)

;--------------------------------------------LOW HELIO-LATITUDE-----------------------------------------------------------------------------
				    xx3=fltarr(ss)
				    yy3=fltarr(ss)
				    xxl3=fltarr(ss)
				    xxd3=fltarr(ss)
				    ss3=0

				   for i=0,ss-1 do begin
					  if (abs(xxl(i)) ge gamma3 && abs(xxl(i)) lt gamma1) then begin
					    xx3(ss3)=xx(i)                     ;fractional year
					    yy3(ss3)=yy(i)
					    xxl3(ss3)=xxl(i)                   ;heliographic latitude
					    xxd3(ss3)=xxd(i)                   ;distance
					    ss3=ss3+1
					 endif
				   endfor

				   xx3=xx3(0:ss3-1)
				   yy3=yy3(0:ss3-1)
				   xxl3=xxl3(0:ss3-1)
				   xxd3=xxd3(0:ss3-1)

;- Computing avgyear

				     xxn3=fltarr(10000)
				     yyn3=fltarr(10000)

				jj3=0

				   for j3=1983,2017,1 do begin                                             ;- loop over all years

					 ny=j3

					   sumx3=0.0
					   sumy3=0.0
					 ncount3=0                                                         ;- initilize no of pts per year

				       for i=0,ss3-1 do begin
					  if(fix(xx3(i)) eq ny)then begin
						sumx3=sumx3+xx3(i)
					       sumy3=sumy3+yy3(i)
					     ncount3=ncount3+1
					  endif
				       endfor
					    if(ncount3 ne 0)then begin

					       xxn3(jj3)=sumx3/float(ncount3)
						yyn3(jj3)=sumy3/float(ncount3)
					      jj3=jj3+1
					    endif
				   endfor


				   xxn3=xxn3(0:jj3-1)
				   yyn3=yyn3(0:jj3-1)

;*************************************** annual means for low latitude obs. for high latitude sources *********************************
				   data1(0:jj3-1,i_source)=xxn3(0:jj3-1)
				   data2(0:jj3-1,i_source)=yyn3(0:jj3-1)
				   ndata_arr[i_source]=jj3

					;print,data1(*,i_source)
					;help,i_source
;**************************************************************************************************************************************


  			endif else begin

;-------------------------------- Compute annual means for all selected observations in the whole helio-latitude range-----------------

				     xxn=fltarr(10000)
				     yyn=fltarr(10000)

				jj=0

				   for j=1983,2017,1 do begin                                             ;- loop over all years

					 ny=j
					   sumx=0.0
					   sumy=0.0

					 ncount=0                                                         ;- initilize no of pts per year

				       for i=0,ss-1 do begin
					  if(fix(xx(i)) eq ny)then begin
					     sumx=sumx+xx(i)
					       sumy=sumy+yy(i)
					     ncount=ncount+1
					  endif
				       endfor
					    if(ncount ne 0)then begin
					      xxn(jj)=sumx/float(ncount)
						yyn(jj)=sumy/float(ncount)
					      jj=jj+1
					    endif
				   endfor

				   xxn=xxn(0:jj-1)
				   yyn=yyn(0:jj-1)
;******************************* annual means for low latitude sources ***************************************************************
					data1(0:jj-1,i_source)=xxn(0:jj-1)
					data2(0:jj-1,i_source)=yyn(0:jj-1)
					ndata_arr[i_source]=jj

					;Print,data1(*,i_source)
					;help,i_source
;*************************************************************************************************************************************


			endelse

;----------------------------------------------PLOTTING SYMBOL-----------------------------------------------------------------------

;- Using Greek Symbols in postscript files(Programs from D. Fanning)

   thisLetter = "145B
   greekLetter = '!9' + String(thisLetter) + '!X'

;- Using Greek Symbols in postscript outputs(Programs from Craig W.)

        textps=textoidl('\leq',font=0)

;- sunsymbol
        solarradius='R' + sunsymbol()

;----------------------------------------------USED DEFINED CIRCLE--------------------------------------------------------------------
				A = FINDGEN(17) * (!PI*2/16.)                        ;-  Make a vector of 16 points, A[i] = 2pi/16

				USERSYM, COS(A), SIN(A),THICK=8,/fill                ;-  The following code illustrates the use of
						                                     ;- USERSYM to define a new symbol-a FILLED circle.

				LOADCOLORS                                           ;- Load colors.

;- color=0 -black -- color=5 -red -- color=6 -blue

;----------------------------------------------Labeling y-axis------------------------------------------------------------------------

				if (nplot eq yval) then begin                        ;-Set ytick values according to yrange.
				   mytickv=[0.5,0.6,0.8,0.85]
				   mytickname=[' ','0.6','0.8',' ']
				   yval=yval+3
				endif else begin
				   mytickv=[0.5,0.6,0.8,0.85]
				   mytickname=[' ',' ',' ',' ']
				endelse


				 val=m*n-m-1                                          ;- Set xtick values
				 val2=val+3
				 if (nplot gt val && nplot le val2) then begin        ;- Set xtick values for last row of the multiplot.
				   ;xmytickv=[1984,1992,2000,2008]
				   ;xmytickname=[' ',' ',' ',' ']
				   xmytickv=[1984,1992,2000,2008,2016]
				   xmytickname=['1984','1992','2000','2008','2016']
				 endif else begin
				   xmytickv=[1984,1992,2000,2008,2016]
				   xmytickname=[' ',' ',' ',' ',' ']
				 endelse


;-----------------------------------------Condition for plotting high and low latitude observations-----------------------------------------

 				if(max(abs(xxl)) gt 45) then begin

           				usersym,cos(A), sin(A), thick=8,color=0,/fill

					Plot,xx,yy, $                                             ;- Plot year and scindex.
					       psym=8,symsize=0.25,/xst,/yst, $
					       ycharsize=1.4,xcharsize=1.2,xticklen=0.06,yticklen=0.06, $
					       ytickv=mytickv,yticks=3,ytickname=mytickname,yminor=3,$
;					       xtickv=xmytickv,xticks=4,xtickname=xmytickname,xminor=2,$
;					       xrange=[1982,2017],xtickinterval=8,yrange=[0.5,0.85], $
					       xrange=[1982,2017],xtickinterval=8,yrange=[0.35,1.25], $
					       charsize =1.4,xthick=10.0,ythick=10.0,charthick=10,thick=10, $
					       /ylog,color=0,/nodata

					 OPlot,xx3,yy3,psym=1,symsize=0.85,color=13,thick=4       ;- Overplot low latitude obs.(grey-crosses)

					 usersym,cos(A), sin(A), thick=8,color=5,/fill

					 OPlot,xx2,yy2,psym=8,symsize=0.35                        ;- Overplot high latitude obs.(red-dots).

					 USERSYM, COS(A), SIN(A),THICK=8

					 OPlot,xxn3,yyn3,psym=8,symsize=1.5,color=6,thick=8       ;- Overplot annual means for low lat. (blue circle)

				endif else begin

					usersym,cos(A), sin(A), thick=8,color=0,/fill

					Plot,xx,yy, $                                             ;- Plot year and scindex.
					       ;ytitle='SC-index(Arb.units)',xtitle='Year',
					       psym=8,symsize=0.25,/xst,/yst, $
					       ycharsize=1.4,xcharsize=1.2,xticklen=0.06,yticklen=0.06, $
					       ytickv=mytickv,yticks=3,ytickname=mytickname,yminor=3,$
;					       xtickv=xmytickv,xticks=4,xtickname=xmytickname,xminor=2,$
;					       xrange=[1982,2017],xtickinterval=8,yrange=[0.5,0.85], $
					       xrange=[1982,2017],xtickinterval=8,yrange=[0.35,1.25], $
					       charsize =1.4,xthick=10.0,ythick=10.0,charthick=10,thick=10, $
					       /ylog,color=0,/nodata

					OPlot,xx,yy,psym=1,symsize=0.85,color=13,thick=4          ;- Overplot low lat. obs.(symbol '+')

					usersym,cos(A), sin(A), thick=8,color=6

					OPlot,xxn,yyn,psym=8,symsize=1.5                          ;- Overplot annual means for all lat. obs.(blue circles)

				endelse

;--------------------------------------------PRINTING-----------------------------------------------------------------------------------

					     xyouts,1984,0.53,''+SOURCE[I_SOURCE]+'',charsize=2.2,charthick=10          ;- Print sources used.
					     xyouts,1986,0.77,'0.26!N '+textps+'     '+textps+' 0.82 AU', $
					     charsize=2.2,charthick=10,font=0
					     xyouts,1995,0.77,'r',charsize=2.8,charthick=10,font=0

					     ;Print,'source plotted:',source[i_source],i_source                         ;- Source Plotted.

;---------------------------------------------------------------------------------------------------------------------------------------
                                	nplot=nplot+1                                         ;- Increment plot number.
                                        npanel=npanel+1
;- One set of plots with m*n is done, here is the condition for next set of plots.

					if (nplot eq nplt) then begin
					       device,/close

                                          if (npanel lt n_sources) then begin	                      ;- Condition for next set of plots
						nplot=0
                                                yval=0
						t=t+1
						ts=strcompress(t,/remove_all)
						;outfile='norm-scindex-'+ts+'.eps'
						outfile='norm-scindex-'+ts+'-1.eps'
						device,filename=outfile,xsize=35,ysize=35,bits=64,scale_factor=1.0,$
						       /encapsulated,/color,yoffset=0.1,xoffset=0.1

						!p.multi=[0,m,n]
						multiplot,xgap=0.005,ygap=0.0025, $
							  mytitle='Normalized Scintillation Index (m)', myTitSize=2.6, myTitOffset=1.2, $
							  mxtitle='Year', mxTitSize=2.1, mxTitOffset=1.2
					  endif else begin
						   device,/close
		                                   Set_plot,'x'
	  		                           multiplot,/default
	  		                          !p.multi=0
                                          endelse

					endif else begin

;- If one set of plots is yet to be completed, then the program code steps here.

						multiplot,xgap=0.005,ygap=0.0025
					endelse

;---------------------------------------------------------------------------------------------------------------------------------------
                           endif                                            ;- close condition for minimum no. of observations.

                        endfor                                              ;- close Loop for each source
;----------------------------------------------END MULTIPLOT ENVIORNMENT----------------------------------------------------------------

;- Plot annual means for all 27 sources

			data1=data1[0:max(ndata_arr)-1,*]                          ;- assigned values for annual years for each source
			data2=data2[0:max(ndata_arr)-1,*]                          ;- assigned values for annual means for each source

			ndata11=dblarr(10000)
			ndata22=dblarr(10000)

				kk=0
				for i_sourcei=0,n_sources-1 do begin
				  ndatai=ndata_arr[i_sourcei]
				  ndata11(kk:kk+ndatai-1)=data1(0:ndatai-1,i_sourcei)
				  ndata22(kk:kk+ndatai-1)=data2(0:ndatai-1,i_sourcei)
					 kk=kk+ndatai
				endfor

				ndata11=ndata11(0:kk-1)
				ndata22=ndata22(0:kk-1)

			        xx4=dblarr(kk)
			        yy4=dblarr(kk)

				xx4(0:kk-1)=ndata11(0:kk-1)
				yy4(0:kk-1)=ndata22(0:kk-1)

;- Write normalized annual means of sc. index. for all 27 sources.

		openw,lun,'annualmeans-scindex.txt',/get_lun
		for i=0,kk-1 do begin
		printf,lun,xx4(i),yy4(i),format='(2x,f14.4,2x,f14.8)'
		endfor
		free_lun,lun
;---------------------------PLOT-----------------------------------------

                		set_plot,'ps'

			  	outfile='annual-means-plot.eps'
                                ;outfile='annual-means-plot-color.eps'

				device,filename=outfile,xsize=25,ysize=15,bits=64,scale_factor=1.0,$
				      /encapsulated,/color,yoffset=1.0,xoffset=1.0
				!p.multi=0

				LOADCOLORS

					   	mytickv=[0.5,0.6,0.7,0.8,0.85]
					   	mytickname=['0.5','0.6','0.7','0.8',' ']

					   	xmytickv=[1982,1990,1998,2006,2014]
					   	xmytickname=['1982','1990','1998','2006','2014']


					   	textps=textoidl('\leq',font=0)

					   	usersym,cos(A), sin(A), thick=8;,color=6;,/fill   ;- Change color

;- Select data points for first source only
					   	ndata0=ndata_arr[0]
					  	ndata10=dblarr(ndata0,1)
					   	ndata20=dblarr(ndata0,1)

					   	ndata10(*,0)=data1(0:ndata0-1,0)
					   	ndata20(*,0)=data2(0:ndata0-1,0)

;- Plot data points for first source only

						plot,ndata10[*,0],ndata20[*,0], position=[0.1,0.15,0.94,0.95],$
							       ytitle= 'Scintillation Index (m)',xtitle='Year', $
							       psym=8,symsize=0.25,/xst,/yst, $
							       ycharsize=1.4,xcharsize=1.4,xticklen=0.04,yticklen=0.04, $
							       ytickv=mytickv,yticks=4,ytickname=mytickname,yminor=5,$
							       xtickv=xmytickv,xticks=4,xtickname=xmytickname,$
							       xrange=[1982,2017], $
							       ;xtickinterval=8, $
							       yrange=[0.42,0.85],xminor=4, $
							       charsize =1.3,xthick=8.0,ythick=8.0,charthick=8,thick=8, $
							       /ylog,color=0,/nodata, font=0

						oplot,ndata10[*,0],ndata20[*,0],psym=8,symsize=0.75,color=0,thick=8

						for i_source=1,n_sources-1 do begin
								  ndata=ndata_arr[i_source]
								  ndata1=dblarr(ndata,n_sources)
								  ndata2=dblarr(ndata,n_sources)

								  ndata1(*,i_source)=data1(0:ndata-1,i_source)
								  ndata2(*,i_source)=data2(0:ndata-1,i_source)

						      oplot,ndata1[*,i_source],ndata2[*,i_source],psym=8,symsize=0.75,color=0,thick=4

						endfor


						plots,[1996.5,0.77],psym=8,symsize=3,color=0

						xyouts,1998.2,0.76,'Annual means of m', $
							     charsize=2.2,charthick=8,font=0
						xyouts,1995,0.8,'Total no. of sources = 27', $
							     charsize=2.2,charthick=8,font=0


				device,/close

		        	Set_plot,'x'
	  		    	multiplot,/reset
	  		    	!p.multi=0

;--------------------------------------COMPUTE ANNUAL MEANS FOR ANNUAL MEANS OF ALL 27 SOURCES----------------------------------
;- Computing annual avg values with sigma on means (one way).

             ss4=kk

				   xxn4=dblarr(35)
				   yyn4=dblarr(35)

				   jj4=0

				   for j4=1983,2017,1 do begin                                             ;- loop over all years

					 ny=j4
					   sumx4=0.0
					   sumy4=0.0

					 ncount4=0                                                         ;- initilize no of pts per year

				       for i=0,ss4-1 do begin
					  if(fix(xx4(i)) eq ny)then begin
					     sumx4=sumx4+xx4(i)
					       sumy4=sumy4+yy4(i)
					     ncount4=ncount4+1
					  endif
				       endfor
					    if(ncount4 ne 0)then begin
					      xxn4(jj4)=sumx4/float(ncount4)
						yyn4(jj4)=sumy4/float(ncount4)
					      jj4=jj4+1
					    endif
				   endfor

				   xxn4=xxn4(0:jj4-1)
				   yyn4=yyn4(0:jj4-1)

;-----------------------------------------------------------------------------
;- Computing annual avg values with sigma on means (other way).

             ss5=kk
             xxn5=dblarr(35)
	     yyn5=dblarr(35)
             err5=dblarr(35)
             xx4_arr=fltarr(1000,35)
             yy4_arr=fltarr(1000,35)
             ncount4_arr=fltarr(35)

           jj5=0
           for j5=1983,2017,1 do begin                              ;- loop over years.
               ny=j5
               ncount5=0                                            ;- initilize no. of pts.

               for i=0,ss5-1 do begin
                  if(xx4(i) ge ny && xx4(i) lt ny+1)then begin      ;- Choosing data points subjected to condition.
                     xx4_arr(ncount5,jj5)=xx4(i)
                     yy4_arr(ncount5,jj5)=yy4(i)
                     ncount5=ncount5+1
                  endif
               endfor
                   ncount4_arr[jj5] = ncount5
                   jj5=jj5+1
           endfor
                     xx4_arr=xx4_arr(0:max(ncount4_arr)-1,*)
                     yy4_arr=yy4_arr(0:max(ncount4_arr)-1,*)

           tt=0
           for k=0,jj5-1 do begin
                    if (ncount4_arr(k) ne 0) then begin
                     xxn5(tt)=mean(xx4_arr(0:ncount4_arr[k]-1,k))
                     yyn5(tt)=mean(yy4_arr(0:ncount4_arr[k]-1,k))
                     err5(tt)=sqrt(variance(yy4_arr(0:ncount4_arr[k]-1,k)))
                     tt=tt+1
                    endif
           endfor

           xxn5=xxn5(0:tt-1)  ;- annual years
           yyn5=yyn5(0:tt-1)  ;- annual means for m
           err5=err5(0:tt-1)  ;- sigma on annual means for m
           err6=fltarr(35)
           err6(0:tt-5)=err5(0:tt-5)
;           err6(tt-4)=0.7*(err5(tt-4))
;           err6(tt-3)=0.7*(err5(tt-3))
;           err6(tt-2)=0.7*(err5(tt-3))
;           err6(tt-1)=0.7*(err5(tt-1))
	    err6(tt-4)=1.0*(err5(tt-4))
           err6(tt-3)=1.0*(err5(tt-3))
           err6(tt-2)=1.0*(err5(tt-3))
           err6(tt-1)=1.0*(err5(tt-1))
;-------------------------------------------------------------------------------------
;- Write avg. annual means.
		openw,lun,'avg-annualmeans-scindex.txt',/get_lun
		for ii=0,tt-1 do begin
		printf,lun,xxn5(ii),yyn5(ii),err6(ii),format='(2x,f14.4,2x,f14.8,2x,f14.7)'
		endfor
		free_lun,lun
;-------------------------------------------------------------------------------------
;- LINEAR FIT TO AVG. ANNUAL MEANS OF SC. INDEX DATA

				n2=33           ;- n2+1 is the size of xxn4
				n1=9
				rows2=(n2-n1)+1 ;- n1 is the point from where we need to start the fit. Here n1=1992.57

				year3=fltarr(1,rows2)
				avg=fltarr(1,rows2)

				year3=xxn4(n1:n2)
				avg=yyn4(n1:n2)

				Print,'Initial point of the linear fit', year3(0)
                                Print,'Final point of the linear fit', year3 (rows2-1)

;- Use LINFIT function to fit data
				coeff3 = LINFIT(year3,avg)
;- YFIT3 is the fitted line:
				yfit3 = coeff3[0]+coeff3[1]*year3

 				Print, 'Slope of the fitted line', coeff3[1]

;- Find the x value of the corresponding y value (=0.5) using the slope.

;				xavg=[year3(rows2-1)]-[(yfit3(rows2-1)-0.5)/float(coeff3[1])]
;				Print,'The x-value to the corresponding y-value', xavg

;- Extrapolate the fitted line up to 2020 and find value of sc. index at 2020.

				y2020=yfit3(rows2-1)+(float(coeff3[1]))*(2020.0-year3(rows2-1))

   				Print,'The value of sc. index at 2020', y2020
;------------------------------ OUTOUT-----------------------
;Solar elongation range in deg=      15.0701 to       55.0848
;Distance from sun in au=     0.260000 to      0.820000
;Initial point of the linear fit       1992.5767
;Final point of the linear fit       2017.6483
;Slope of the fitted line   -0.0076546467
;The value of sc. index at 2020      0.49372564
;Corresponding source size for the value sc. index 0.49
;compute alog10(0.49) and compare with the value of sc.index at eps. 15 from mtheo_eps2.all and 
;find the source size.
;The computed source size is 150 mas
;---------------------------------- PLOT ANNUAL MEANS AND AVG. ANNUAL MEANS ---------------------------------------------------------------------
			    	set_plot,'ps'

			  	outfile='annual-means-plot-final-AA.eps'

				device,filename=outfile,xsize=25,ysize=15,bits=64,scale_factor=1.0,$
				      /encapsulated,/color,yoffset=1.0,xoffset=1.0
				!p.multi=0

				LOADCOLORS

					   mytickv=[0.5,0.6,0.7,0.75]
					   mytickname=['0.5','0.6','0.7',' ']

					   xmytickv=[1982,1990,1998,2006,2014,2022,2030,2038]
					   xmytickname=['1982','1990','1998','2006','2014','2022','2030','2038']


					   textps=textoidl('\leq',font=0)

					   usersym,cos(A), sin(A),thick=8,color=0,/fill

					   plot,xx4,yy4, position=[0.1,0.15,0.94,0.95],$  ; PLOT ANNUAL MEANS
							       ytitle='Normalized Scintillation Index',xtitle='Year', $
							       psym=8,symsize=0.35, /xst, /yst, $
							       ycharsize=1.35,xcharsize=1.35,xticklen=0.04,yticklen=0.04, $
							       ytickv=mytickv,yticks=3,ytickname=mytickname,yminor=5,$
							       ;xtickv=xmytickv,xticks=7,xtickname=xmytickname,$
							       xrange=[1980,2020],yrange=[0.4265,0.7],xminor=5, $
							       charsize =1.35,xthick=8.0,ythick=8.0,charthick=8,thick=8, $
							       color=0

                        		    usersym,cos(A), sin(A),thick=6,color=6;,/fill            ;- Change color - color = 6
					    oplot,xxn4,yyn4,psym=8,symsize=1.8,color=0               
					    ;oploterror,xxn5,yyn5,err5,psym=8,symsize=1.8,color=0
					    oploterror,xxn5,yyn5,err6,psym=8,symsize=1.8, $
                                            HATLENGTH=400,ERRTHICK=6, ERRSTYLE=0, ERRCOLOR=0

                        		    oplot,year3(2:24),yfit3(2:24),color=5,thick=8,linestyle=0  ;- Change color - color = 5

;- Show the extrapolated fitted line
                        		    plots,[year3(rows2-1),yfit3(rows2-1)]
                        		    plots,[2020,y2020],/continue,thick=8,color=5,linestyle=1  ;- Change color - color = 5
;- Legend

					    usersym,cos(A), sin(A), thick=8,color=6          ;- Change color - color = 6
					    plots,[2008.8,0.714],psym=8,symsize=2

					    xyouts,2009.8,0.708,'Annual means', $
					    charsize=1.8,charthick=8,font=0

;- Mark solar cycles as per online site (List of Solar Cycles wikipedia) 
;  1976.47, 1986.7, 1996.34, 2008.08

                        		    xyouts,1980.2,0.44,'Cycle 21',charsize=1.2,charthick=6.5
					    xyouts,1989,0.44,'Cycle 22',charsize=1.2,charthick=6.5
					    xyouts,1999,0.44,'Cycle 23',charsize=1.2,charthick=6.5
					    xyouts,2012,0.44,'Cycle 24',charsize=1.2,charthick=6.5

                        		    ;vline,1976.47,thick=6,linestyle=3
					    ;vline,1986.7,thick=6,linestyle=3
					    ;vline,1996.34,thick=6,linestyle=3
					    ;vline,2008.08,thick=6,linestyle=3

					    ;arrow,1978.47,0.55,1976.47,0.55,/data,hthick=2,thick=2
					    ;arrow,1985.5,0.443,1986.7,0.443,/data,hthick=2,thick=2
					    ;arrow,1988.7,0.443,1986.7,0.443,/data,hthick=2,thick=2
					    ;arrow,1995,0.443,1996.34,0.443,/data,hthick=2,thick=2
					    ;arrow,1998.34,0.443,1996.34,0.443,/data,hthick=2,thick=2
					    ;arrow,2005.08,0.443,2008.08,0.443,/data,hthick=2,thick=2
					    ;arrow,2011.08,0.443,2008.08,0.443,/data,hthick=2,thick=2
					    ;arrow,2018,0.443,2020,0.443,/data,hthick=2,thick=2

;- Mark new solar cycles accroding to NGDC - 1976, 1986, 1996, 2008

                                            ;vline,1976,thick=6,linestyle=3
                                            vline,1986,thick=6,linestyle=3
                                            vline,1996,thick=6,linestyle=3
                                            vline,2008,thick=6,linestyle=3

					    ;arrow,1978,0.443,1976,0.443,/data,hthick=2,thick=2
					    arrow,1985,0.443,1986,0.443,/data,hthick=2,thick=2
					    arrow,1988,0.443,1986,0.443,/data,hthick=2,thick=2
					    arrow,1994,0.443,1996,0.443,/data,hthick=2,thick=2
					    arrow,1998,0.443,1996,0.443,/data,hthick=2,thick=2
					    arrow,2005,0.443,2008,0.443,/data,hthick=2,thick=2
					    arrow,2011,0.443,2008,0.443,/data,hthick=2,thick=2
					    arrow,2018,0.443,2020,0.443,/data,hthick=2,thick=2

;- Draw a vertical line at 2020.
                                            vline,2020,thick=6,linestyle=2,color=0

				device,/close
		                Set_plot,'x'
	  		        multiplot,/reset
	  		        !p.multi=0
;==========================================================================================================

;- Fitting values
openw,lun,'fit-annualmeans-scindex.txt',/get_lun
for iii=0,rows2-1 do begin
printf,lun,year3(iii),yfit3(iii),format='(2x,f14.4,2x,f14.8)'
endfor
free_lun,lun

stop
end
