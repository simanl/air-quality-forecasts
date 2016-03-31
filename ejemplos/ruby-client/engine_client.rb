require 'rserve'
require 'rserve/simpler'
require 'csv'
require 'byebug'
require 'active_support/all'

# require './engine_client'; cli = EngineClient.new; x = cli.run_test_cycle

class EngineClient

  NUMERIC_FIELDS = %w(CO NO NO2 NOX O3 PM10 PM2.5 PRS RAINF HR SO2 SR TOUT WS WDR)

  attr_reader :conn

  # Example initializer, connects to the remote R host specified in the
  # ENGINE_URL environment variable:
  def initialize
    engine_uri = URI(ENV.fetch 'ENGINE_URL')
    @conn = Rserve::Simpler.new hostname: engine_uri.host
  end

  def load_test_data(file_path)
    file_data = File.read file_path

    data = CSV.parse file_data, headers: true

    # Inicializar el DataFrame:
    data_frame = data.headers.inject({}) do |df, header|
      df[header] = []
      df
    end

    # Llenar el dataframe:
    data.inject(data_frame) do |df, row|
      df.keys.each do |field_name|
        value = row.field(field_name)
        df[field_name] << if value == 'NA' then nil
        elsif field_name == 'hora' then value.to_i
        elsif NUMERIC_FIELDS.include?(field_name) then value.to_f
        else value
        end
      end
      df
    end
  end

  def last_cycle_run_path
    @_last_cycle_run_path ||= File.join TEST_CYCLES_PATH, 'last_cycle_run.txt'
  end

  TEST_CYCLES_PATH = '/sample-data'
  def run_test_cycle(cycle_to_run = nil)

    if cycle_to_run.nil?
      last_cycle_run = if File.exist?(last_cycle_run_path)
        File.read(last_cycle_run_path).to_i
      else
        0
      end
      cycle_to_run = last_cycle_run + 1
    end

    cycle_data_path = File.join TEST_CYCLES_PATH, "ciclo#{cycle_to_run}.csv"
    raise "Sample data file '#{cycle_data_path}' does not exist" unless File.exists?(cycle_data_path)

    print "Running cycle with data from file '#{cycle_data_path}'..."

    cycle_data = load_test_data(cycle_data_path)

    begin
      results = spca(cycle_data)
      exit_message = "finished!"
      File.write(last_cycle_run_path, cycle_to_run.to_s, mode: 'w')
      results
    rescue => e
      exit_message = "FAILED! \n#{e.class.name} - #{e.message}"
    ensure
      puts " #{exit_message}"
    end

  end

  def spca(input_data)
    # Las fechas se interpretan por rserve-simpler como
    # número de días a partir del 1970-01-01:
    # Date.parse('1970-01-01').advance(days: 16222)

    forecast_data = conn.converse("tabla.entrada" => input_data.to_dataframe) do
      'SPCA()'
    end

    # SPCA method completed successfully on engine... tell it to save the data:
    conn.command 'post()' if forecast_data

    # Formar un array de Hashes
    row_size = forecast_data.map { |column| column.size }.uniq.max
    blank_row = forecast_data.names.inject({}) do |hsh, name|
      hsh[name.to_sym] = nil
      hsh
    end

    # Return an array of hashes:
    (0..(row_size-1)).inject([]) do |data, row_index|
      row = blank_row.dup
      row.keys.each do |field_name|
        field_value = forecast_data[field_name.to_s][row_index]

        row[field_name] = case field_name
        when :fecha then Date.parse('1970-01-01').advance(days: field_value.to_i)
        else field_value
        end
      end
      data << row
      data
    end
  rescue Rserve::Connection::EvalError => rserve_error
    raise "The SPCA remote method failed - probably an error with your input data?"
  end

end
