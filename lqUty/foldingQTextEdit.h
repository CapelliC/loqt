/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014,2015

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

#ifndef FOLDINGQTEXTEDIT_H
#define FOLDINGQTEXTEDIT_H

#include <QTextEdit>
#include "framedTextAttr.h"
#include "foldedTextAttr.h"

/**
 * @brief The foldingQTextEdit class
 *  example code to complete my answer to my question on StackOverflow: see
 *  http://stackoverflow.com/questions/19232882/nice-text-formatting-in-qtextedit-like-qtcreator-does
 */
class LQUTYSHARED_EXPORT foldingQTextEdit : public QTextEdit
{
    Q_OBJECT

public:
    foldingQTextEdit();

private:
    framedTextAttr* framed_handler;
    foldedTextAttr* folded_handler;

public slots:
    void fold();
    void unfold();
    void frame();
    void unframe();
};

#endif // FOLDINGQTEXTEDIT_H
