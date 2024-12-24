

import pandas as pd
import os
from datetime import datetime, timedelta

def generate_date_range(start_date, end_date):
    """Generate a date range from start_date to end_date."""
    date_range = pd.date_range(start=start_date, end=end_date, freq='D')
    return date_range

def check_and_fill_missing_dates(file_path, start_date, end_date, output_path):
    """Check and fill missing dates in the text file."""
    # Read the file into a DataFrame
    df = pd.read_csv(file_path, delim_whitespace=True, header=None)

    # Add column names
    columns = ['Year', 'Month', 'Day'] + [f'Col{i}' for i in range(1, len(df.columns) - 2)]
    df.columns = columns

    # Combine Year, Month, and Day into a single Date column
    df['Date'] = pd.to_datetime(df[['Year', 'Month', 'Day']])

    # Check for duplicate dates and remove them
    df = df.drop_duplicates(subset=['Date'])

    # Generate the complete date range
    complete_dates = generate_date_range(start_date, end_date)
    complete_dates_df = pd.DataFrame({
        'Date': complete_dates,
        'Year': complete_dates.year,
        'Month': complete_dates.month,
        'Day': complete_dates.day
    })

    # Merge with the complete date range to find missing dates
    merged_df = pd.merge(complete_dates_df, df, on=['Date', 'Year', 'Month', 'Day'], how='left')

    # Drop the redundant Date column after merging
    merged_df.drop(columns=['Date'], inplace=True)

    # Fill missing values with 'NA'
    merged_df.fillna('NA', inplace=True)

    # Save the updated file
    merged_df.to_csv(output_path, sep=' ', index=False, header=False)

def process_all_files(input_directory, output_directory, start_date, end_date):
    """Process all text files in the input directory."""
    for file_name in os.listdir(input_directory):
        if file_name.endswith('.txt'):
            input_file_path = os.path.join(input_directory, file_name)
            output_file_path = os.path.join(output_directory, file_name)
            print(f"Processing {file_name}...")
            check_and_fill_missing_dates(input_file_path, start_date, end_date, output_file_path)

# Define parameters
input_directory = "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Data/gw_data_ver.2/gw_tk"
output_directory = "/Users/sunpierce/Desktop/Academia Sinica/gw_project/Data/gw_data_ver.2/gw_tk_processed"
start_date = datetime(2008, 4, 1)
end_date = datetime(2023, 3, 31)

# Ensure the output directory exists
os.makedirs(output_directory, exist_ok=True)

# Process all files
process_all_files(input_directory, output_directory, start_date, end_date)
