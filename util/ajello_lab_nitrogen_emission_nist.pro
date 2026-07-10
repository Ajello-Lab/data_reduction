;+
; PURPOSE
;  Read in the atomic emission lines from nitrogen as 
;  taken from the NIST atomic database on Apr 30, 2020.
;-
pro ajello_lab_nitrogen_emission_nist, arr

  ajello_lab_set_paths, path_base, path_repo
  
  ;file = '/Users/holsclaw/data_resources/NIST/NIST_atomic_database_N2_50_200nm.txt'
  file = path_repo + 'data/N/NIST_atomic_database_N_50_200nm_tab_delimited.txt'
  ;data = (read_ascii(file,data_start=1,delimiter=',')).(0)
  nlines = file_lines(file)-1
  openr,fid,file,/get_lun
  str = ''
  struct = {ion:0,wave_obs:0.,wave_ritz:0.,rel_int:0.}
  arr = replicate(struct,nlines)
  readf,fid,str ; read header
  for i = 0, nlines - 1 do begin
    readf,fid,str
    temp = strsplit(str,string(9B),/extract)
    arr[i].ion = temp[1]
    ;
    pos1 = strpos(temp[2],'"')
    pos2 = strpos(temp[2],'"',/reverse_search)
    if pos2-pos1 gt 1 then arr[i].wave_obs = strmid(temp[2],pos1+1,pos2-pos1-1)
    ;
    pos1 = strpos(temp[3],'"')
    pos2 = strpos(temp[3],'"',/reverse_search)
    if pos2-pos1 gt 1 then arr[i].wave_ritz = strmid(temp[3],pos1+1,pos2-pos1-1)
    ;
    pos1 = strpos(temp[4],'"')
    pos2 = strpos(temp[4],'"',/reverse_search)
    if pos2-pos1 gt 1 then arr[i].rel_int = strmid(temp[4],pos1+1,pos2-pos1-1)
  endfor
  close,fid
  free_lun,fid

end