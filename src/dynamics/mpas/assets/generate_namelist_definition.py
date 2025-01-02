#!/usr/bin/env python3

'''
Generate XML namelist definition file for MPAS dynamical core in CAM-SIMA.
'''

import argparse
import textwrap
import xml.etree.ElementTree as ET

# These namelist groups are irrelevant to CAM-SIMA for its particular use case of MPAS.
# Hide them to prevent users from inadvertently modifying them.
EXCLUDED_NAMELIST_GROUP = [
    'assimilation',
    'iau',
    'limited_area',
    'physics',
    'restart'
]
# These namelist options are forcefully controlled by CAM-SIMA at run-time.
# Hide them to prevent users from inadvertently modifying them.
EXCLUDED_NAMELIST_OPTION = [
    'config_calendar_type',
    'config_do_restart',
    'config_run_duration',
    'config_start_time',
    'config_stop_time'
]
# List all overridden namelist options below.
# `OVERRIDDEN_NAMELIST_OPTION` is a dictionary with `str` as keys and `list[str]` as values.
OVERRIDDEN_NAMELIST_OPTION = {
    'mpas_block_decomp_file_prefix': [
        '<value hgrid="mpasa480">${DIN_LOC_ROOT}/atm/cam/inic/mpas/mpasa480.graph.info.part.</value>',
        '<value hgrid="mpasa120">${DIN_LOC_ROOT}/atm/cam/inic/mpas/mpasa120.graph.info.part.</value>',
        '<value hgrid="mpasa60">${DIN_LOC_ROOT}/atm/cam/inic/mpas/mpasa60.graph.info.part.</value>',
        '<value hgrid="mpasa30">${DIN_LOC_ROOT}/atm/cam/inic/mpas/mpasa30.graph.info.part.</value>',
        '<value hgrid="mpasa15">${DIN_LOC_ROOT}/atm/cam/inic/mpas/mpasa15.graph.info.part.</value>',
        '<value hgrid="mpasa12">${DIN_LOC_ROOT}/atm/cam/inic/mpas/mpasa12.graph.info.part.</value>',
        '<value hgrid="mpasa15-3">${DIN_LOC_ROOT}/atm/cam/inic/mpas/mpasa15-3.graph.info.part.</value>'
    ],
    'mpas_coef_3rd_order': [
        '<value>1.0</value>'
    ],
    'mpas_dt': [
        '<value hgrid="mpasa480">1800.0</value>',
        '<value hgrid="mpasa120">900.0</value>',
        '<value hgrid="mpasa60">450.0</value>',
        '<value hgrid="mpasa30">225.0</value>'
    ],
    'mpas_len_disp': [
        '<value hgrid="mpasa480">480000.0</value>',
        '<value hgrid="mpasa120">120000.0</value>',
        '<value hgrid="mpasa60">60000.0</value>',
        '<value hgrid="mpasa30">30000.0</value>'
    ],
    'mpas_number_cam_damping_levels': [
        '<value>0</value>'
    ],
    'mpas_number_rayleigh_damp_u_levels': [
        '<value>5</value>'
    ],
    'mpas_print_detailed_minmax_vel': [
        '<value>.true.</value>'
    ],
    'mpas_rayleigh_damp_u': [
        '<value>.true.</value>'
    ]
}

INDENT_PER_LEVEL = ' ' * 4
NEW_PREFIX = 'mpas_'
OLD_PREFIX = 'config_'

def parse_argument() -> argparse.Namespace:
    '''
    Parse command line arguments.
    '''

    parser = argparse.ArgumentParser(
        description='Generate XML namelist definition file for MPAS dynamical core in CAM-SIMA.'
    )

    parser.add_argument(
        '-r', '--registry',
        default='Registry.xml',
        type=str,
        required=False,
        help='XML MPAS registry file.',
        dest='reg_xml'
    )
    parser.add_argument(
        '-n', '--namelist',
        default='Namelist.xml',
        type=str,
        required=False,
        help='XML CAM-SIMA namelist definition file.',
        dest='nml_xml'
    )
    parser.add_argument(
        '-s', '--schema',
        default=None,
        type=str,
        required=False,
        help='XML schema for CAM-SIMA namelist definition file.',
        dest='nml_xsd'
    )

    argument = parser.parse_args()

    return argument

def parse_xml(xml_file: str) -> ET.ElementTree:
    '''
    Parse XML file into element tree.
    '''

    xml_et = ET.parse(xml_file)

    return xml_et

def validate_xml(xml_file: str, xsd_file: str) -> bool:
    '''
    Validate XML file against XSD file.
    '''

    # Only import `xmlschema` if XML validation is requested. Shush pylint about it.
    import xmlschema # pylint: disable=import-outside-toplevel

    xml_schema = xmlschema.XMLSchema(xsd_file)

    return xml_schema.is_valid(xml_file)

def transform_name(name: str) -> str:
    '''
    Change prefix of namelist option/group name.
    '''

    while name.startswith(OLD_PREFIX):
        name = name[len(OLD_PREFIX):]

    while name.startswith(NEW_PREFIX):
        name = name[len(NEW_PREFIX):]

    name = NEW_PREFIX + name

    return name

def translate_element_tree(reg_xml_et: ET.ElementTree) -> ET.ElementTree:
    '''
    Translate MPAS registry into namelist definition.
    '''

    # `entry_id_pg` is the root element in namelist definition.
    entry_id_pg_element = ET.Element('entry_id_pg', {'version': '0.1'})

    comment_element = ET.Comment(
        '\n' +
        INDENT_PER_LEVEL * 2 + 'MPAS dycore' + '\n' +
        '\n' +
        INDENT_PER_LEVEL * 2 + 'Note to developers/maintainers:' + '\n' +
        INDENT_PER_LEVEL * 2 + 'This file is auto-generated from the MPAS registry. Do not edit directly.' + '\n' +
        INDENT_PER_LEVEL * 2 + 'Instead, use the Python script at `src/dynamics/mpas/assets/generate_namelist_definition.py`.' + '\n' +
        INDENT_PER_LEVEL
    )
    entry_id_pg_element.append(comment_element)

    for namelist_group in reg_xml_et.findall('nml_record'):
        if namelist_group.attrib['name'].strip().lower() in EXCLUDED_NAMELIST_GROUP:
            continue

        for namelist_option in namelist_group.findall('nml_option'):
            if namelist_option.attrib['name'].strip().lower() in EXCLUDED_NAMELIST_OPTION:
                continue

            # The `entry_id_pg` root element contains many `entry` elements.
            # Each `entry` element describes a namelist option, indicated by its `id` attribute.
            entry_element = ET.SubElement(entry_id_pg_element, 'entry', {'id': transform_name(namelist_option.attrib['name'].strip().lower())})

            # The `category` element.
            category_element = ET.SubElement(entry_element, 'category')
            category_element.text = 'mpas'

            # The `desc` element.
            desc_text = ' '.join(namelist_option.attrib['description'].strip(' .').split())
            desc_text = desc_text[0].upper() + desc_text[1:]
            desc_text = '\n' + textwrap.fill(desc_text, 80, initial_indent=INDENT_PER_LEVEL * 3, subsequent_indent=INDENT_PER_LEVEL * 3) + '\n' + INDENT_PER_LEVEL * 2

            desc_element = ET.SubElement(entry_element, 'desc')
            desc_element.text = desc_text

            # The `group` element.
            group_element = ET.SubElement(entry_element, 'group')
            group_element.text = transform_name(namelist_group.attrib['name'].strip().lower())

            # The `type` element.
            # The `values` element and its containing `value` element.
            type_text = namelist_option.attrib['type'].strip().lower()
            value_text = namelist_option.attrib['default_value']

            # Do some sanitization.
            if type_text.startswith('ch'):
                type_text = 'char*256'
                value_text = value_text.strip()

                if not value_text.isascii():
                    raise ValueError('"' + value_text + '" is not ASCII')
            elif type_text.startswith('in'):
                type_text = 'integer'
                value_text = canonicalize_int(value_text)
            elif type_text.startswith('lo'):
                type_text = 'logical'
                value_text = value_text.strip().lower()

                if value_text.startswith(('t', '.t')):
                    value_text = '.true.'
                elif value_text.startswith(('f', '.f')):
                    value_text = '.false.'
                else:
                    raise ValueError('"' + value_text + '" does not represent a logical')
            elif type_text.startswith('re'):
                type_text = 'real'
                value_text = canonicalize_real(value_text)
            else:
                raise ValueError('"' + type_text + '" is not a supported type')

            type_element = ET.SubElement(entry_element, 'type')
            type_element.text = type_text

            values_element = ET.SubElement(entry_element, 'values')

            if entry_element.attrib['id'] in OVERRIDDEN_NAMELIST_OPTION:
                for value_string in OVERRIDDEN_NAMELIST_OPTION[entry_element.attrib['id']]:
                    value_element = ET.fromstring(value_string)
                    values_element.append(value_element)
            else:
                value_element = ET.SubElement(values_element, 'value')
                value_element.text = value_text

    # Sort the `entry` elements for result stability except for the comment element at index 0.
    entry_id_pg_element[1:] = sorted(entry_id_pg_element.iterfind('entry'), key=lambda entry: entry.get('id'))
    nml_xml_et = ET.ElementTree(entry_id_pg_element)

    return nml_xml_et

def canonicalize_int(s: str) -> str:
    '''
    Canonicalize a string that represents an integer.
    '''

    try:
        i = int(s)
    except Exception as e:
        raise ValueError('"' + s + '" does not represent an integer') from e

    return str(i)

def canonicalize_real(s: str) -> str:
    '''
    Canonicalize a string that represents a real.
    '''

    try:
        f = float(s)
    except Exception as e:
        raise ValueError('"' + s + '" does not represent a real') from e

    return str(f)

def write_element_tree(xml_file: str, xml_et: ET.ElementTree) -> None:
    '''
    Write element tree into XML file.
    '''

    ET.indent(xml_et, space=INDENT_PER_LEVEL)

    xml_et.write(
        xml_file,
        encoding='UTF-8',
        xml_declaration=True,
        default_namespace=None,
        method='xml',
        short_empty_elements=False
    )

    # The file written by `ElementTree.write()` contains no newline at end of file. Add it manually.
    with open(xml_file, 'a', encoding='utf-8') as nml_xml_file:
        nml_xml_file.write('\n')

if __name__ == '__main__':
    arg = parse_argument()
    reg_xml_element_tree = parse_xml(arg.reg_xml)
    nml_xml_element_tree = translate_element_tree(reg_xml_element_tree)

    write_element_tree(arg.nml_xml, nml_xml_element_tree)
    print('Generated ' + arg.nml_xml)

    if arg.nml_xsd is not None:
        if validate_xml(arg.nml_xml, arg.nml_xsd):
            print('Successfully validated ' + arg.nml_xml + ' against ' + arg.nml_xsd)
        else:
            print('Failed to validate ' + arg.nml_xml + ' against ' + arg.nml_xsd)
