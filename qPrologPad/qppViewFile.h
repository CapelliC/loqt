/*
    qPrologPad   : SWI-Prolog PrologPad in Qt

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2014 Carlo Capelli

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

#ifndef QPPVIEWFILE_H
#define QPPVIEWFILE_H

#include "qppView.h"

/** boilerplate to handle editing a source file with CodeMirror
 *  and semantic highlighting from PrologPad
 */
class qppViewFile : public qppView
{
    Q_OBJECT

public:

    qppViewFile(QWidget *parent = 0);
    qppViewFile(const qppViewFile &copy);

    //! make a backup copy on write ?
    Q_PROPERTY(bool makeBackup READ makeBackup WRITE setMakeBackup)
    bool makeBackup() const { return makeBackup_; }
    void setMakeBackup(bool yn) { makeBackup_ = yn; }

    //! property path
    Q_PROPERTY(QString path READ path WRITE setPath)
    QString path() const { return path_; }
    void setPath(QString path) { path_ = path; }

    //! load <path> to textarea
    Q_INVOKABLE bool loadFile(QString path);

    //! dump textarea to <path>
    Q_INVOKABLE bool saveFile();

protected:

    bool error(QString msg);

    QString path_;
    bool makeBackup_;
};

#endif // QPPVIEWFILE_H
