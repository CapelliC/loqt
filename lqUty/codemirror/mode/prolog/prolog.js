/**!
lqUty        : loqt utilities

Author       : Carlo Capelli
E-mail       : cc.carlo.cap@gmail.com
Copyright (C): 2013, Carlo Capelli

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/
CodeMirror.defineMode("prolog", function() {
  return {
    startState: function() {
      return { in_comment:0 };
    },
    token: function(stream,state) {
      var token_name;
      if (state.in_comment) {
        token_name = 'comment';
        if (stream.match("*/"))
          state.in_comment--;
        else if (stream.match("/*"))
          state.in_comment++;
        else
          stream.next();
      } else if (stream.match("/*")) {
        token_name = 'comment';
        state.in_comment++;
      } else if (stream.match(/[a-z][A-Za-z0-9_]*/))
        token_name = 'atom';
      else if (stream.match(/\'(\\\'|[^\'])*\'/))
        token_name = 'atom';
      else if (stream.match(/\"(\\\"|[^\"])*\"/))
        token_name = 'string';
      else if (stream.match(/0'./))
        token_name = 'string-2';
      else if (stream.match(/[A-Z_][A-Za-z0-9_]*/))
        token_name = 'variable-2';
      else if (stream.match(/[0-9]+(\.[0-9]+)?/))
        token_name = 'number';
      else if (stream.match(/[\+\-\*\/\=\^<>~:\.\?@#$\\&{}`]+/))
        token_name = 'operator';
      else
        if (stream.next() === '%') {
          stream.skipToEnd();
          token_name = 'comment';
        }
      return token_name;
    }
  };
});
