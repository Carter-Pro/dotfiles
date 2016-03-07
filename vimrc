set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

call vundle#end()            " required
filetype plugin indent on    " required

" 显示行号
set nu

" 高亮当前行
set cursorline

" 自动缩进
set autoindent
set smartindent
set smarttab
set cindent

" 缩进宽度
set tabstop=4
set shiftwidth=4

" 语法高亮
syntax on
