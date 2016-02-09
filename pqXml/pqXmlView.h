/*
    pqXml        : loqt XML utilities

    Author       : Carlo Capelli
    E-mail       : cc.carlo.cap@gmail.com
    Copyright (C): 2015

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

#ifndef PQXMLVIEW_H
#define PQXMLVIEW_H

#include "foldingQTextEdit.h"
#include "pqXml_global.h"

/**
 * @brief The pqXmlView class
 *
 *  A foldable, editable XML text view, to handle SVG,SCXML,.glade,.ui, and XML of course.
 *  The main requisite is that SWI-Prolog library(sgml):load_xml/3 can parse the loaded file,
 *  thus making semantic syntax highlighting available.
 *  library(xpath) will provide extensive pattern matching and processing.
 */
class PQXMLSHARED_EXPORT pqXmlView : public foldingQTextEdit
{
    Q_OBJECT
public:
    pqXmlView();
    pqXmlView(QString path);

    QString file;
    void openFile(QString file);

signals:

public slots:

};

#endif // PQXMLVIEW_H
