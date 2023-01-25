/*
    lqUty        : loqt utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright @ 2023

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
    CodeMirrorFile(const CodeMirrorFile &copy);

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

Q_DECLARE_METATYPE(CodeMirrorFile)

#endif // CODEMIRRORFILE_H
