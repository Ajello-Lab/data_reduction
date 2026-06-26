pro messing
  compile_opt idl2

  ; a1 = [0, 0, 1, 1, 0, 0, 1, 1]
  ; a2 = [0, 1, 0, 1, 0, 1, 0, 1, 0]
  ; a3 = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i']

  ; a4 = a3[where(a1 and a2)]

  ; a5 = a3.contains('b')
  ; print, a4
  ; print, a1 and a2
  ; print, a5

  ; path = '/Users/benjamincondit/Desktop/IUVS_Breadboard/Round 12/data_reduction/'
  ; type = '.png'
  ; k1 = '16EV'
  ; k2 = '16EV'
  ; folder_searcher, path, type, k1, k2, files
  ; print, files

  ; temp = strsplit(files[0], '/', /extract)
  ; print, temp[-1]

  ; list_test = list()
  ; list_test1 = [[1, 3, 4, 6, 8], [2, 3, 3, 2, 3]]
  ; list_test1 = transpose(list_test1)
  ; list_test2 = [[7, 4, 3, 2, 1], [8, 9, 7, 6, 10]]
  ; list_test2 = transpose(list_test2)
  ; list_test.add, list_test1
  ; list_test.add, list_test2

  ; list_test.add, transpose([[6, 4, 5, 0, 3], [8, 9, 7, 6, 10]])
  ; list_test.add, transpose([[10, 11, 8, 5, 2], [12, 20, 5, 11, 13]])
  ; ; print, list_test[1]
  ; list_test = list_test.toArray()

  ; colors = ['Blue', 'Firebrick', 'Green Yellow', 'Hot Pink', 'Yellow', 'Dark Magenta', 'Dark Green', 'Chocolate', 'Indigo', 'light sky blue']

  ; win = window(dim = [1400, 600])
  ; for i = 0, (size(list_test, /dimensions))[0] - 1 do begin
  ; if i eq 10 then begin
  ; print, 'Attempted to plot too many lines'
  ; stop
  ; endif
  ; x = list_test[i, 0, *]
  ; y = list_test[i, 1, *]
  ; print, i
  ; p = plot(x, y, color = colors[i], overplot = p, current = win)
  ; endfor

  newarr = list()
  newarr.add, 'hi'
  newarr.add, 'hello'

  ; newarr = list()
  print, newarr
end
