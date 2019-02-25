let s:tbl = [
\  ['stmts',
\    [
\      { 'type': 'node', 'match': ['stmt', ['lb', 'stmt']], 'eval': 's:stmts' },
\    ],
\  ],
\  ['stmt',
\    [
\      { 'type': 'node', 'match': ['expr', ['|', 'expr']], 'eval': 's:stmt' },
\    ],
\  ],
\  ['expr',
\    [
\      { 'type': 'node', 'match': ['expr_node', 'sp', '==', 'sp', 'expr_node'], 'eval': 's:op_eqeq' },
\      { 'type': 'node', 'match': ['if', 'sp', 'expr', 'lb', 'end'], 'eval': 's:expr_if' },
\      { 'type': 'node', 'match': ['ident', '(', ')'], 'eval': 's:op_call' },
\      { 'type': 'node', 'match': ['ident', '(', 'expr_node', ')'], 'eval': 's:op_call' },
\      { 'type': 'node', 'match': ['ident', 'sp', '=', 'sp', 'expr_node'], 'eval': 's:op_let' },
\      { 'type': 'node', 'match': ['ident', 'sp', '+=', 'sp', 'expr_node'], 'eval': 's:op_plus' },
\      { 'type': 'node', 'match': ['expr_node', ['sp', '|', 'sp', 'expr_node']], 'eval': 's:expr' },
\      { 'type': 'node', 'match': ['expr_node'], 'eval': 's:expr' },
\    ],
\  ],
\  ['expr_node',
\    [
\      { 'type': 'node', 'match': ['{', 'sp', '|', 'ident', '|', 'sp', '}'], 'eval': 's:expr_func' },
\      { 'type': 'node', 'match': ['{', 'sp', '|', 'ident', '|', 'sp', 'stmts', 'sp', '}'], 'eval': 's:expr_func' },
\      { 'type': 'node', 'match': ['ident'], 'eval': 's:expr' },
\      { 'type': 'node', 'match': ['number'], 'eval': 's:expr' },
\      { 'type': 'node', 'match': ['string'], 'eval': 's:expr' },
\    ],
\  ],
\  ['string', [{ 'type': 'regexp', 'match': '\("[^"]*"\|''[^'']*''\)', 'eval': 's:expr_string' }]],
\  ['number', [{ 'type': 'regexp', 'match': '[0-9]\+', 'eval': 's:expr_number' }]],
\  ['ident', [{ 'type': 'regexp', 'match': '[a-zA-Z][a-zA-Z0-9]*', 'eval': 's:expr_ident' }]],
\  ['+=', [{ 'type': 'string', 'match': '+=', 'eval': '' }]],
\  ['=', [{ 'type': 'string', 'match': '=', 'eval': '' }]],
\  ['|', [{ 'type': 'string', 'match': '|', 'eval': '' }]],
\  ['if', [{ 'type': 'string', 'match': 'if', 'eval': '' }]],
\  ['end', [{ 'type': 'string', 'match': 'end', 'eval': '' }]],
\  ['sp', [{ 'type': 'regexp', 'match': '[ \t]*', 'eval': '' }]],
\  ['lb', [{ 'type': 'regexp', 'match': '[ \t]*[\\r\n;]\+[ \t]*', 'eval': '' }]],
\  ['{', [{ 'type': 'string', 'match': '{', 'eval': '' }]],
\  ['}', [{ 'type': 'string', 'match': '}', 'eval': '' }]],
\  ['(', [{ 'type': 'string', 'match': '(', 'eval': '' }]],
\  [')', [{ 'type': 'string', 'match': ')', 'eval': '' }]],
\]

let s:debug = 0

if s:debug
  function! s:debug(...) abort
    echo join(a:000, ' ')
  endfunction
else
  function! s:debug(...) abort
  endfunction
endif

function! s:chomp(x) abort
  return filter(a:x, '!empty(v:val)')
endfunction

function! s:op_call(value) abort
  return { 'type': 'op_call', 'value': s:chomp(a:value) }
endfunction

function! s:op_let(value) abort
  return { 'type': 'op_let', 'value': s:chomp(a:value) }
endfunction

function! s:op_eqeq(value) abort
  return { 'type': 'op_eqeq', 'value': s:chomp(a:value) }
endfunction

function! s:op_plus(value) abort
  return { 'type': 'op_plus', 'value': s:chomp(a:value) }
endfunction

function! s:stmt(value) abort
  return { 'type': 'stmt', 'value': s:chomp(a:value) }
endfunction

function! s:stmts(value) abort
  return { 'type': 'stmts', 'value': s:chomp(a:value) }
endfunction

function! s:expr(value) abort
  return {'type': 'expr', 'value': s:chomp(a:value) }
endfunction

function! s:expr_if(value) abort
  return {'type': 'if', 'value': s:chomp(a:value) }
endfunction

function! s:expr_ident(value) abort
  return {'type': 'ident', 'value': a:value }
endfunction

function! s:expr_func(value) abort
  return {'type': 'func', 'value': s:chomp(a:value) }
endfunction

function! s:expr_number(value) abort
  return {'type': 'number', 'value': str2nr(a:value) }
endfunction

function! s:expr_string(value) abort
  return {'type': 'string', 'value': eval(a:value) }
endfunction

let s:level = 0

function! s:parse_node(ctx, node) abort
  try
    let s:level += 1
    if type(a:node) == 1
      if a:ctx.pos == len(a:ctx.str)
        return ''
      endif
      let pos = a:ctx.pos
      call s:debug(repeat(' ', s:level), 'TRY', string(a:node), string(a:ctx.str[a:ctx.pos:]))
      for item in s:tbl
        if a:node == item[0]
          for n in item[1]
            let a:ctx.pos = pos
            if n.type ==# 'string'
              if stridx(a:ctx.str[a:ctx.pos:], n.match) == 0
                let a:ctx.pos += len(n.match)
                call s:debug(repeat(' ', s:level), 'HIT', string(n))
                if n.eval !=# ''
                  return call(n.eval, [n.match])
                else
                  return {}
                endif
              endif
            elseif n.type ==# 'regexp'
              if a:ctx.str[a:ctx.pos:] =~# '^' . n.match
                let m = matchstr(a:ctx.str[a:ctx.pos:], '^' . n.match)
                let a:ctx.pos += len(m)
                call s:debug(repeat(' ', s:level), 'HIT', string(n))
                if n.eval !=# ''
                  return call(n.eval, [m])
                else
                  return {}
                endif
              endif
            elseif n.type ==# 'node'
              let s = s:parse_node(a:ctx, n)
              if type(s) == 3
                call s:debug(repeat(' ', s:level), 'HIT', string(n))
                return call(n.eval, [s])
              elseif type(s) == 4
                call s:debug(repeat(' ', s:level), 'HIT', string(n))
                return s
              endif
              unlet s
            endif
          endfor
        endif
      endfor
      call s:debug(repeat(' ', s:level), 'NG ', string(a:node), string(a:ctx.str[a:ctx.pos:]))
      "let a:ctx.pos = pos
      return ''
    elseif type(a:node.match) == 3
      let value = []
      let deep = []
      let pos = a:ctx.pos
      let i = 0
      while i < len(a:node.match)
        let n = a:node.match[i]
        if type(n) == 3
          let j = 0
          while 1
            let nn = n[j % len(n)]
            let vv = s:parse_node(a:ctx, nn)
            if type(vv) ==# 1 && vv ==# ''
              return value
            endif
            call add(value, vv)
            if a:ctx.str[a:ctx.pos:] ==# ''
              return value
            endif
            unlet vv
            let j += 1
          endwhile
          return value
        endif
        let v = s:parse_node(a:ctx, n)
        if type(v) ==# 1 && v ==# ''
          let a:ctx.pos = pos
          return ''
        endif
        call add(value, v)
        unlet v
        let i += 1
        unlet n
        if a:ctx.str[a:ctx.pos:] ==# ''
          if i < len(a:node.match) - 1
            let a:ctx.pos = pos
            return ''
          else
            return value
          endif
        endif
      endwhile
      if len(value) < len(a:node.match)
        let a:ctx.pos = pos
        return ''
      endif
      return value
    endif
    return ''
  finally
    let s:level -= 1
  endtry
endfunction

function! s:invoke(node, env) abort
  if a:node.type ==# 'stmts'
    let x = 0
    for stmt in a:node.value
      unlet x
      let x = s:invoke(stmt, a:env)
    endfor
    return x
  elseif a:node.type ==# 'stmt'
    let x = 0
    for stmt in a:node.value
      unlet x
      let x = s:invoke(stmt, a:env)
    endfor
    return x
  elseif a:node.type ==# 'expr'
    " streem
    let res = s:invoke(a:node.value[0], a:env)
    let a:env['input'] = res
    for n in a:node.value[1:]
      unlet res
      let res = s:invoke(n, a:env)
      let a:env['input'] = res
    endfor
    return res
  elseif a:node.type ==# 'func'
    let name = a:node.value[0].value
    let lines = a:env['input']
    let res = []
    if len(a:node.value) > 1
      for l in lines
        let a:env['vars'][name] = l
        let r = s:invoke(a:node.value[1], a:env)
        call add(res, r)
        unlet r
      endfor
    else
      let res = lines
    endif
    return res
  elseif a:node.type ==# 'ident'
    if a:node.value ==# 'STDIN'
      if mode() =~# '[vV]'
        return getline(a:line1, a:line2)
      endif
      return ['']
    endif
    if a:node.value ==# 'STDOUT'
      echo join(a:env['input'], "\n")
      return []
    endif
    return a:env.vars[a:node.value]
  elseif a:node.type ==# 'op_call'
    let args = map(deepcopy(a:node.value[1].value), 's:invoke(v:val, a:env)')
    return call(a:env[a:node.value[0].value], args, a:env)
  elseif a:node.type ==# 'op_let'
    let name = a:node.value[0].value
    let value = s:invoke(a:node.value[1], a:env)
    let a:env['vars'][name] = value
    return value
  elseif a:node.type ==# 'op_plus'
    let name = a:node.value[0].value
    let value = s:invoke(a:node.value[1], a:env)
    if type(value) != 1
      let v = string(value)
      unlet value
      let value = v
    endif
    if has_key(a:env['vars'], 'name')
      let a:env['vars'][name] .= value
    else
      let a:env['vars'][name] = value
    endif
    return a:env['vars'][name]
  endif
  return a:node.value
endfunction

function! s:parse_tree(ctx) abort
  let stmts = s:parse_node(a:ctx, 'stmts')
  if a:ctx.pos < len(a:ctx.str)
    throw a:ctx.str[a:ctx.pos:]
  endif
  return stmts
endfunction

function! s:parse(value) abort
  let ctx = {'str': a:value, 'pos': 0}
  return s:parse_tree(ctx)
endfunction

function! s:streem(line1, line2, value) abort
  "echo a:line1 a:line2 a:value
  if mode() =~# '[vV]'
    let env = {'vars': {}, 'input': getline(a:line1, a:line2)}
  else
    let env = {'vars': {}, 'input': ['']}
  endif
  function! env.map(x) abort
    return map()
  endfunction
  function! env.print(...) abort
    echo join(a:000, ' ')
    return a:000
  endfunction
  let ast = s:parse(a:value)
  return s:invoke(ast, env)
endfunction

command! -range -nargs=* Streem call s:streem(<line1>, <line2>, <q-args>)

"Streem {|x| x += "foo"} | STDOUT

"call s:streem(1, 1, "STDIN|{|x|print('hello world')}|STDOUT")
"call s:streem(1, 1, "STDIN|{|x|x+=3}|STDOUT")
"call s:streem(1, 1, "STDIN | {|x|} | STDOUT")
"call s:streem(1, 1, "STDIN | {|x| x += 1 } | STDOUT")
"call s:streem(1, 1, "{|x| if x == 2; end}")
"call s:streem(1, 1, "{|x|x = 2}")
"call s:streem(1, 1, "{|x|}")
"call s:streem(1, 1, "STDIN | 3 | 4")
"call s:streem(1, 1, "x+='3'")
"call s:streem(1, 1, "x")
"call s:streem(1, 1, "STDIN")

"Streem {|x|
"\  x += "foo";
"\  print(x)
"\ } | STDOUT

" vim:set et
