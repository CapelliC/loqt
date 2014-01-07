/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2013,2014 Carlo Capelli

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

#ifndef CODEMIRRORFILE_H
#define CODEMIRRORFILE_H

#include "CodeMirror.h"

/** boilerplate to handle editing a source file with CodeMirror
 */
class LQUTYSHARED_EXPORT CodeMirrorFile : public CodeMirror
{
    Q_OBJECT

public:

    CodeMirrorFile(QWidget *parent = 0);
    CodeMirrorFile(const CodeMirrorFile &file);

    Q_INVOKABLE bool loadFile(QString path);

protected:

    QString path;
};

Q_DECLARE_METATYPE(CodeMirrorFile)

#endif // CODEMIRRORFILE_H
