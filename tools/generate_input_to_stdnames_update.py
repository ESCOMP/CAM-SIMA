"""

"""

import argparse
import csv
import re
from collections import defaultdict
from bs4 import BeautifulSoup


def parse_csv(csv_filepath):
    """Returns a dictionary of standard names (keys) to input names from snapshots (set value)

    The current spreadsheet currently uses column 0 as the input name and column 6 as the standard name.
    Currently only using 432 lines in the spread sheet that need standard names.

    Ex:

    .. code-block::

      A1,...,...,...,...,A6,...
      B1,...,...,...,...,A6,...
      C1,...,...,...,...,C5,...

      ->

      { A6: (A1, B1), C5: (C1)

    """
    datamap = defaultdict(set)
    pattern = re.compile(r"\w+")
    print(f"Opening {csv_filepath}")
    with open(csv_filepath, encoding='ascii') as csvfile:
        csvdata = csv.reader(csvfile)
        for row in csvdata:
            inputname = row[0].split(" ")[0]
            standardname_match = pattern.fullmatch(row[5].split(" ")[0])
            if csvdata.line_num < 432 and standardname_match and inputname and "Skipping" not in row[5] and "CCPP" not in row[5]:
                print(f"Adding {inputname} under {standardname_match.string}")
                datamap[standardname_match.string].add(inputname)
    return datamap



def generate_stdname_xml(current_dict, output_filename):
    """
    Generates an xml file to be used by a converter script that converts
    input names in a snapshot file to standard names.
    
    For example,

    .. code-block::

      { A6: (A1, B1), C5: (C1)

    would be converted to:

    .. code-block:: XML

      <entries>
        <entry stdname="A6">
          <ic_file_input_names>
            <ic_file_input_name>A1</ic_file_input_name>
            <ic_file_input_name>B1</ic_file_input_name>
          <ic_file_input_names>
        </entry>
        <entry stdname="C5">
          <ic_file_input_name>C1</ic_file_input_name>
        </entry>
      </entries>

    """
    xmltree = BeautifulSoup(features="xml")

    entries = xmltree.new_tag("entries")
    for k, v in current_dict.items():
        entry = xmltree.new_tag("entry")
        entry["stdname"] = k
        names = xmltree.new_tag("ic_file_input_names")
        for name in v:
            namenode = xmltree.new_tag("ic_file_input_name")
            namenode.string = name
            names.append(namenode)
        entry.append(names)
        entries.append(entry)
    xmltree.append(entries)
    with open(output_filename, "w", encoding='ascii') as xmlout:
        print(f"Creating new xml file : {output_filename}")
        xmlout.write(xmltree.prettify())


def main():
    """
    Parses a CSV file with a column ordering of

    .. code-block::

      <snapshot file/input name>,...,...,...,...,<standard name>,...
    
    and generates a corresponding xml file of the format

    .. code-block:: XML
    
      <entries>
        <entry stdname="stdname">
          <ic_file_input_name>input_name</ic_file_input_name>
          ...
        </entry>
        ...
      </entries>
 
    """
    parser = argparse.ArgumentParser(description='')
    parser.add_argument('--csv-file', type=str, default='CCPP Standard Names - Sheet1.csv', help='')
    parser.add_argument('--output-map', type=str, default='stdnames_to_inputnames_dictionary_new.xml', help='')

    args = parser.parse_args()

    current_csv_entries = parse_csv(args.csv_file)
    generate_stdname_xml(current_csv_entries, args.output_map)


if __name__=="__main__":
    main()
