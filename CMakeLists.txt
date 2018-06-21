#-------------------------------------------------
# Trasm
#-------------------------------------------------

macro(add_pascal_target name)
	set(${name}oName "${CMAKE_CURRENT_BINARY_DIR}/${name}")
	set(${name}extra_argv ${ARGN})
	list(LENGTH extra_argv extra_argc)
	if(1 LESS extra_argc)
		message(FATAL_ERROR "arguments is too few.")
	endif(1 LESS extra_argc)

	if("Windows" STREQUAL ${CMAKE_SYSTEM_NAME})
		set(${name}oName "${${name}oName}.exe")
	endif("Windows" STREQUAL ${CMAKE_SYSTEM_NAME})

	set(${name}_FLAGS "${CMAKE_PASCAL_FLAGS}")
	if("Debug" STREQUAL ${CMAKE_BUILD_TYPE})
		set(${name}_FLAGS ${${name}_FLAGS} ${CMAKE_PASCAL_FLAGS_DEBUG})
	elseif("Release" STREQUAL ${CMAKE_BUILD_TYPE})
		set(${name}_FLAGS ${${name}_FLAGS} ${CMAKE_PASCAL_FLAGS_RELEASE})
	endif()

	set(${name}SRCS "")
	set(${name}OBJS "")
	foreach(arg IN LISTS ${name}extra_argv)
		get_filename_component(${name}obj arg NAME_WE)
		set(${name}obj "${${name}obj}.o")
		set(${name}SRCS "${${name}SRCS};${CMAKE_CURRENT_SOURCE_DIR}/${arg}")
		set(${name}OBJS "${${name}OBJS};${CMAKE_CURRENT_BINARY_DIR}/${${name}obj}")
	endforeach() # 

	add_custom_target(${name}_bin ALL
		SOURCES "${${name}oName}")

	message(STATUS "prjbin: ${${name}oName}")
	set(PASCMD ${CMAKE_PASCAL_COMPILER};-o\"${${name}oName}\";${${name}_FLAGS};${${name}SRCS})

	# Link
	add_custom_command(OUTPUT "${${name}oName}"
		COMMAND ${CMAKE_PASCAL_COMPILER} -o\"${${name}oName}\" ${${name}_FLAGS} ${${name}SRCS}
		DEPENDS ${${name}SRCS}
		)

	unset(${name}SRCS)
	unset(${name}OBJS)
	unset(${name}oName)
endmacro(add_pascal_target name)

cmake_minimum_required(VERSION 3.6)
project(trasm NONE)

if(NOT CMAKE_BUILD_TYPE)
        message(STATUS "No build type selected, default to Release")
        set(CMAKE_BUILD_TYPE "Release" CACHE STRING 
                "Choose the type of build, options are: Debug Release RelWithDebInfo MinSizeRel." FORCE)
endif(NOT CMAKE_BUILD_TYPE)


set(CMAKE_PASCAL_FLAGS "")
if(UNIX)
	set(CMAKE_PASCAL_COMPILER fpc)
elseif(MINGW)
	set(CMAKE_PASCAL_COMPILER ppcross386)
	set(CMAKE_PASCAL_FLAGS ${CMAKE_PASCAL_FLAGS} -Twin32 -XPi386-win32)
endif()

if(UNIX OR MINGW)
	set(CMAKE_PASCAL_FLAGS ${CMAKE_PASCAL_FLAGS} -Mtp)
	set(CMAKE_PASCAL_FLAGS_DEBUG \$-g)
	set(CMAKE_PASCAL_FLAGS_RELEASE -O2 -Xd -Xs -CX)
endif(UNIX OR MINGW)

#set(TrasmSources
#	src/trcommon.pas
#	src/trutils.pas
#	src/trerror.pas
#	)

add_pascal_target(trasm src/trasm.pas ${TrasmSources})
add_pascal_target(trasm_b src/trasm_b.pas ${TrasmSources})

