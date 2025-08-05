;+
; NAME:
; AJELLO_LAB_SET_PATHS
; 
; PURPOSE:
; This routine will define the base path to the local IUVS datasets for an identified user.
; 
; INPUTS:
; none.
; 
; OUTPUTS:
; none.
;-
pro ajello_lab_set_paths, path_base, path_repo

;  defsysv,'!path_data',exists=exists1
;  defsysv,'!path_data2',exists=exists2
;  
;  if ~exists1 or ~exists2 then begin
    
    login = get_login_info()
    case login.user_name of
      'holsclaw': begin
        path_base = '/Volumes/projects/Phase_Development/MAVEN/IUVS_Data/IUVS_Breadboard/'        
        ;path_base = '/Users/holsclaw/MAVEN/Ajello_lab/'
      end
      'rele2355': begin
        ;path_base = 'w:\documents\IBM\Mobi\maven\'
        path_base = 'Z:\IUVS_Breadboard\'
      end
      'cpmalone': begin
        path_base = 'F:\_MAVEN\IUVS_Breadboard\'
      end
      'lufa5942': begin
        path_base = "Z:\round10\"
      end
      else:begin
        print, 'Unknown user: Create a new entry in this case statement'
        stop   
      end
    endcase
    
    defsysv,'!path_base',path_base

    file = routine_filepath()
    path_repo = file_dirname(file,/MARK_DIRECTORY)
    DEFSYSV, '!path_repo', path_repo
  
end