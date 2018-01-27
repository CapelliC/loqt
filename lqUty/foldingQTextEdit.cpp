/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015,2016,2017,2018

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

#include "foldingQTextEdit.h"

foldingQTextEdit::foldingQTextEdit()
    : framed_handler(new framedTextAttr(this))
    , folded_handler(new foldedTextAttr(this))
{
    document()->documentLayout()->registerHandler(framed_handler->type(), framed_handler);
    document()->documentLayout()->registerHandler(folded_handler->type(), folded_handler);
    setLineWrapMode(NoWrap);
}

void foldingQTextEdit::fold() {
    folded_handler->fold(textCursor());
}
void foldingQTextEdit::unfold() {
    folded_handler->unfold(textCursor());
}

void foldingQTextEdit::frame() {
    framed_handler->frame(textCursor());
}
void foldingQTextEdit::unframe() {
    framed_handler->unframe(textCursor());
}
